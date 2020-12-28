module Editor.RoofManager where

import Prelude hiding (add,degree)

import Algorithm.MeshFlatten (flattenRoofPlates)
import Algorithm.PointInPolygon (underPolygons)
import Control.Alt ((<|>))
import Control.Monad.Reader (ask)
import Control.Plus (empty)
import Data.Array as Array
import Data.Compactable (compact)
import Data.Default (def)
import Data.Foldable (class Foldable, foldl, sequence_, traverse_)
import Data.Lens (Lens', view, (^.), (%~), (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), concat, toUnfoldable)
import Data.List as List
import Data.Map (Map, delete, insert, lookup, values)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID)
import Data.UUIDMap (UUIDMap)
import Data.UUIDMap as UM
import Editor.ArrayBuilder (runArrayBuilder)
import Editor.Common.Lenses (_alignment, _deleted, _disposable, _face, _geometry, _houseId, _id, _mesh, _modeDyn, _mouseMove, _orientation, _panelType, _point, _position, _roof, _roofId, _roofs, _tapped, _updated, _verticeTree, _wrapper)
import Editor.Disposable (class Disposable, dispose)
import Editor.EditorMode (EditorMode(..))
import Editor.House (HouseMeshData)
import Editor.HouseEditor (ArrayEditParam, HouseEditor, _heatmap, performEditorEvent)
import Editor.ObjectAdder (ObjectAdder, _addedPoint, _faceNormal, createObjectAdder, mkCandidatePoint)
import Editor.PanelLayer (_currentPanels, _initPanels, _mainOrientation, _roofActive, _serverUpdated)
import Editor.PanelNode (PanelOpacity(..))
import Editor.Rendering.PanelRendering (_opacity)
import Editor.RoofNode (RoofNode, RoofNodeConfig, createRoofNode)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, gateDyn, step)
import FRP.Event (Event, create, fold, keepLatest, sampleOn, subscribe, withLast)
import FRP.Event.Extra (debounce, delay, distinct, mergeArray, multicast, performEvent, skip)
import Math.Angle (acos, degreeVal)
import Model.Racking.OldRackingSystem (OldRoofRackingData, guessRackingType)
import Model.Racking.RackingType (RackingType(..))
import Model.Roof.Panel (Alignment(..), Orientation(..), Panel, PanelsDict, generalOrientation, panelsDict)
import Model.Roof.RoofPlate (RoofEdited, RoofOperation(..), RoofPlate, _roofIntId, newRoofPlate, toRoofEdited)
import Model.RoofSpecific (_value)
import Rendering.Node (Node, mkNodeEnv, runNode)
import Three.Core.Face3 (normal)
import Three.Core.Object3D (class IsObject3D, Object3D, add, mkObject3D, remove, setName, worldToLocal)
import Three.Math.Vector (mkVec3, (<.>))

newtype RoofManager = RoofManager {
    wrapper       :: Object3D,
    editedRoofs   :: Event (Array RoofEdited),
    alignment     :: Event (Maybe Alignment),
    orientation   :: Event (Maybe Orientation),
    serverUpdated :: Event Unit,
    disposable    :: Effect Unit
}

derive instance newtypeRoofManager :: Newtype RoofManager _

instance disposableRoofManager :: Disposable RoofManager where
    dispose r = r ^. _disposable

_editedRoofs :: Lens' RoofManager (Event (Array RoofEdited))
_editedRoofs = _Newtype <<< prop (SProxy :: SProxy "editedRoofs")

type RoofDict = UUIDMap RoofPlate

-- internal data structure used to manage roofs
newtype RoofDictData = RoofDictData {
    roofs         :: RoofDict,       -- all roofs manaaged, will be updated on any changes
    roofsToRender :: Maybe RoofDict  -- roofs used for rerenderring
}

derive instance newtypeRoofDictData :: Newtype RoofDictData _

_roofsToRender :: Lens' RoofDictData (Maybe RoofDict)
_roofsToRender = _Newtype <<< prop (SProxy :: SProxy "roofsToRender")

-- mark all roofs to be rendered
renderAll :: RoofDictData -> RoofDictData
renderAll r = r # _roofsToRender .~ Just (r ^. _roofs)

-- | update the managed roof dict with new operation
applyRoofOp :: RoofOperation -> RoofDictData -> RoofDictData
applyRoofOp (RoofOpCreate roof) rd = renderAll $ rd # _roofs %~ insert (roof ^. _id) roof
applyRoofOp (RoofOpDelete rid)  rd = renderAll $ rd # _roofs %~ delete rid
applyRoofOp (RoofOpUpdate roof) rd = rd # _roofs %~ insert (roof ^. _id) roof
                                        # _roofsToRender .~ Nothing

doFlatten :: HouseMeshData -> RoofDict -> Effect Unit
doFlatten meshData rd = flattenRoofPlates (meshData ^. _geometry) 
                                          (meshData ^. _verticeTree)
                                          (meshData ^. (_mesh <<< _mesh))
                                          (toUnfoldable $ values rd)

-- | accumulate event value in a foldable list of values with the specified retreive function
foldEvtWith :: forall a b f. Foldable f => Functor f => (a -> Event b) -> f a -> Event b
foldEvtWith f l = foldl (<|>) empty (f <$> l)

-- | get roofUpdate event from an array of roof nodes
getRoofUpdate :: forall f. Foldable f => Functor f => f RoofNode -> Event RoofOperation
getRoofUpdate = foldEvtWith (view _updated)

-- | get roofDelete event from an array of roof nodes
getRoofDelete :: forall f. Foldable f => Functor f => f RoofNode -> Event RoofOperation
getRoofDelete = foldEvtWith (view _deleted)

-- | get the activated roof id event from an array of roof nodes
getActivated :: forall f. Foldable f => Functor f => f RoofNode -> Event UUID
getActivated = foldEvtWith f
    where f n = const (n ^. _roofId) <$> (n ^. _tapped)

createWrapper :: Effect Object3D
createWrapper = do
    wrapper <- mkObject3D
    setName "roof wrapper" wrapper
    pure wrapper

-- | function to create roof node
mkNode :: Event (Maybe UUID) -> PanelsDict -> Map Int OldRoofRackingData -> RoofNodeConfig -> RoofPlate -> HouseEditor RoofNode
mkNode activeRoof panelsDict racks cfg roof = runArrayBuilder rackTypeDyn roofNodeBuilder
    where roofNodeBuilder = createRoofNode $ cfg # _roof       .~ roof
                                                 # _roofActive .~ roofActive
                                                 # _initPanels .~ delay 100 (pure ps)

          rid = roof ^. _id
          ps  = fromMaybe Nil (lookup rid panelsDict)
          rackType    = fromMaybe XR10 $ guessRackingType <$> lookup (roof ^. _roofIntId) racks
          rackTypeDyn = pure rackType
          roofActive  = step false $ (==) (Just rid) <$> activeRoof

-- helper function to delete and dispose an old roof node
delOldNode :: forall a. IsObject3D a => a -> RoofNode -> Effect Unit
delOldNode wrapper rn = do
    dispose rn
    remove rn wrapper

-- delete old nodes and add new ones to the wrapper
renderNodes :: forall a. IsObject3D a => a -> { last :: Maybe (Array RoofNode), now :: Array RoofNode } -> Effect (Array RoofNode)
renderNodes wrapper { last, now } = do
    traverse_ (delOldNode wrapper) $ fromMaybe [] last
    traverse_ (flip add wrapper) now
    pure now

calcMainOrientation :: forall f. Foldable f => Functor f => f RoofNode -> Event Orientation
calcMainOrientation nodes = calcOrient <<< concat <<< List.fromFoldable <$> mergeArray (Array.fromFoldable $ view _currentPanels <$> nodes)
    where calcOrient ps = generalOrientation $ view _orientation <$> ps

-- | render dynamic roofs
renderRoofs :: forall a. IsObject3D a => a
                                      -> ArrayEditParam
                                      -> Event (Maybe UUID)
                                      -> Event RoofDictData
                                      -> PanelsDict
                                      -> Map Int OldRoofRackingData
                                      -> HouseEditor (Tuple (Event (Array RoofNode)) (Effect Unit))
renderRoofs wrapper param activeRoof roofsData panelsDict racks = do
    { event: mainOrientE, push: pushMainOrient } <- liftEffect create

    houseId <- view _houseId <$> ask

    let rsToRenderArr = UM.toUnfoldable <$> compact (view _roofsToRender <$> roofsData)
        
        mainOrientDyn = step Landscape $ distinct mainOrientE
        orientDyn     = step Landscape $ param ^. _orientation
        alignDyn      = step Grid $ param ^. _alignment
        opacityDyn    = step Opaque $ param ^. _opacity
        panelTypeDyn  = pure def

        -- base config for roof node
        cfg = def # _houseId         .~ houseId
                  # _mainOrientation .~ mainOrientDyn
                  # _orientation     .~ orientDyn
                  # _alignment       .~ alignDyn
                  # _panelType       .~ panelTypeDyn
                  # _opacity         .~ opacityDyn
                  # _heatmap         .~ (param ^. _heatmap)
    
    -- create roofnode for each roof and render them
    nodes <- performEditorEvent $ traverse (mkNode activeRoof panelsDict racks cfg) <$> rsToRenderArr
    let nodesEvt      = multicast  $ performEvent $ renderNodes wrapper <$> withLast nodes
        mainOrientEvt = keepLatest $ calcMainOrientation <$> nodesEvt
    
    d <- liftEffect $ subscribe mainOrientEvt pushMainOrient

    pure $ Tuple nodesEvt d

isRoofEditing :: HouseEditor (Dynamic Boolean)
isRoofEditing = map ((==) RoofEditing) <<< view _modeDyn <$> ask

-- | function to add the roof recognizer and recognize new roofs
recognizeNewRoofs :: forall e . HouseMeshData -> Object3D -> Event RoofDict -> Dynamic (Maybe UUID) -> Dynamic Boolean -> Node e ObjectAdder
recognizeNewRoofs meshData wrapper newRoofs activeRoof canEditRoofDyn = createObjectAdder point canShowAdder
    where canShowAdder = (&&) <$> (isNothing <$> activeRoof) <*> canEditRoofDyn
          houseWrapper = meshData ^. _wrapper

          up             = mkVec3 0.0 0.0 1.0
          calcAngle norm = acos $ norm <.> up
          validNormal e  = degreeVal (calcAngle (normal $ e ^. _face)) < 60.0
          
          getCandidatePoint evt rs = do
              np <- worldToLocal (evt ^. _point) houseWrapper
              if not (underPolygons rs np) && validNormal evt
                  then pure $ Just $ mkCandidatePoint np (normal (evt ^. _face))
                  else pure Nothing
        
          mouseMoveEvt = meshData ^. _mesh <<< _mouseMove
          point = step Nothing $ performEvent $ sampleOn newRoofs (getCandidatePoint <$> gateDyn canShowAdder mouseMoveEvt)

getActiveRoof :: HouseMeshData -> Event UUID -> Event RoofOperation -> Event RoofPlate -> Event (Maybe UUID)
getActiveRoof meshData activated deleteRoofOp addedNewRoof =
                (const Nothing <$> meshData ^. _mesh <<< _tapped) <|>
                (Just <$> activated) <|>
                (delay 1 $ const Nothing <$> deleteRoofOp) <|>
                (delay 1 $ Just <<< view _id <$> addedNewRoof)

-- helper function that will merge an array of events into a single event.
-- it will use debounce to make sure all events fired very near in any item of
-- the array will be calculated only once.
mergeArrEvt :: forall a b. (a -> Event (Maybe b)) -> Array a -> Event (Maybe b)
mergeArrEvt f arr = g <$> debounce (Milliseconds 50.0) (mergeArray (f <$> arr))
    where g = foldl (<|>) Nothing

-- | create RoofManager for an array of roofs
createRoofManager :: ArrayEditParam -> HouseMeshData -> Array RoofPlate -> Array Panel -> Map Int OldRoofRackingData -> HouseEditor RoofManager
createRoofManager param meshData roofs panels racks = do
    wrapper <- liftEffect createWrapper

    -- create an event stream for the current active id
    { event : activeRoof, push : updateActive }    <- liftEffect create
    { event : roofsData,  push : updateRoofsData } <- liftEffect create

    let activeRoofDyn = step Nothing activeRoof

        -- get the default roof plates as a dict
        defRoofDict = UM.fromFoldable roofs
        psDict      = panelsDict panels
    canEditRoofDyn <- isRoofEditing

    -- render roofs dynamically
    Tuple renderedNodes d <- renderRoofs wrapper param activeRoof roofsData psDict racks

    let deleteRoofOp  = multicast  $ keepLatest $ getRoofDelete <$> renderedNodes
        updateRoofOp  = keepLatest $ getRoofUpdate              <$> renderedNodes
        activatedRoof = keepLatest $ getActivated               <$> renderedNodes
        serverUpdEvt  = multicast $ keepLatest $ foldEvtWith (view _serverUpdated) <$> renderedNodes
        alignEvt      = multicast $ keepLatest $ mergeArrEvt (view _alignment)     <$> renderedNodes
        orientEvt     = multicast $ keepLatest $ mergeArrEvt (view _orientation)   <$> renderedNodes

        -- event of new roofs that will be updated on any change and
        -- run the roof flatten algorithm whenever there's new roof change
        newRoofs  = multicast $ view _roofs <$> roofsData
        flattened = performEvent $ doFlatten meshData <$> newRoofs

    -- recognize new roofs
    Tuple adder adderDisp <- liftEffect $ runNode (recognizeNewRoofs meshData wrapper newRoofs activeRoofDyn canEditRoofDyn) (mkNodeEnv wrapper unit)
    let mkRoof p     = newRoofPlate (p ^. _position) (p ^. _faceNormal)
        addedNewRoof = performEvent $ mkRoof <$> adder ^. _addedPoint
        addRoofOp    = RoofOpCreate <$> addedNewRoof
        ops          = addRoofOp <|> deleteRoofOp <|> updateRoofOp
    
    -- manage all roofs and update it with user operations.
    let defRoofData = RoofDictData { roofs: defRoofDict, roofsToRender: Just defRoofDict }
        roofData = fold applyRoofOp ops defRoofData
    d1 <- liftEffect $ subscribe roofData updateRoofsData

    let newActRoof = getActiveRoof meshData activatedRoof deleteRoofOp addedNewRoof
    d2 <- liftEffect $ subscribe newActRoof updateActive

    -- make sure the serverupdate event is subscribed so API calls will be called
    d3 <- liftEffect $ subscribe serverUpdEvt (const $ pure unit)
    
    -- set default values
    liftEffect do
        updateRoofsData defRoofData
        updateActive Nothing

    let getRoofEdited = map toRoofEdited <<< UM.toUnfoldable

    -- skipe the first roof in teh editedRoofs event, because it's the default data
    pure $ RoofManager {
        wrapper       : wrapper,
        editedRoofs   : multicast $ skip 1 $ debounce (Milliseconds 1000.0) $ getRoofEdited <$> newRoofs,
        alignment     : map (view _value) <$> alignEvt,
        orientation   : map (view _value) <$> orientEvt,
        serverUpdated : serverUpdEvt,
        disposable    : sequence_ [d, d1, d2, dispose adderDisp]
    }
