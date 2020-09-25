module Editor.RoofManager where

import Prelude hiding (add,degree)

import Algorithm.MeshFlatten (flattenRoofPlates)
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
import Data.Map (Map, delete, fromFoldable, insert, lookup, values)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID)
import Data.Unfoldable (class Unfoldable)
import Editor.ArrayBuilder (runArrayBuilder)
import Editor.Common.Lenses (_alignment, _disposable, _geometry, _id, _mesh, _modeDyn, _mouseMove, _orientation, _panelType, _panels, _roof, _roofId, _roofs, _tapped, _verticeTree, _wrapper)
import Editor.Disposable (class Disposable, dispose)
import Editor.EditorMode (EditorMode(..))
import Editor.House (HouseMeshData)
import Editor.HouseEditor (HouseEditor, _roofPlates, performEditorEvent)
import Editor.PanelLayer (_currentPanels, _initPanels, _mainOrientation, _roofActive)
import Editor.PanelNode (PanelOpacity(..))
import Editor.Rendering.PanelRendering (_opacity)
import Editor.RoofNode (RoofNode, RoofNodeConfig, _roofDelete, _roofUpdate, createRoofNode)
import Editor.RoofRecognizer (RoofRecognizer, _addedNewRoof, createRoofRecognizer)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, step)
import FRP.Event (Event, create, fold, keepLatest, subscribe, withLast)
import FRP.Event.Extra (debounce, delay, mergeArray, multicast, performEvent, skip)
import Model.Racking.OldRackingSystem (OldRoofRackingData, guessRackingType)
import Model.Racking.RackingType (RackingType(..))
import Model.Roof.Panel (Alignment(..), Orientation(..), PanelsDict, generalOrientation, panelsDict)
import Model.Roof.RoofPlate (RoofEdited, RoofOperation(..), RoofPlate, _roofIntId, toRoofEdited)
import Three.Core.Object3D (class IsObject3D, Object3D, add, mkObject3D, remove, setName)

newtype RoofManager = RoofManager {
    wrapper     :: Object3D,
    editedRoofs :: Event (Array RoofEdited),
    disposable  :: Effect Unit
}

derive instance newtypeRoofManager :: Newtype RoofManager _

instance disposableRoofManager :: Disposable RoofManager where
    dispose r = r ^. _disposable

_editedRoofs :: Lens' RoofManager (Event (Array RoofEdited))
_editedRoofs = _Newtype <<< prop (SProxy :: SProxy "editedRoofs")

type RoofDict = Map UUID RoofPlate

roofDict :: forall f. Foldable f => Functor f => f RoofPlate -> RoofDict
roofDict = fromFoldable <<< map f
    where f r = Tuple (r ^. _id) r

dictToUnfoldable :: forall f. Unfoldable f => RoofDict -> f RoofPlate
dictToUnfoldable = toUnfoldable <<< values

-- internal data structure used to manage roofs
newtype RoofDictData = RoofDictData {
    roofs :: RoofDict,  -- all roofs manaaged, will be updated on any changes
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

-- | get roofUpdate event from an array of roof nodes
getRoofUpdate :: Array RoofNode -> Event RoofOperation
getRoofUpdate ns = foldl (<|>) empty (view _roofUpdate <$> ns)

-- | get roofDelete event from an array of roof nodes
getRoofDelete :: Array RoofNode -> Event RoofOperation
getRoofDelete ns = foldl (<|>) empty (view _roofDelete <$> ns)

-- | get the activated roof id event from an array of roof nodes
getActivated :: Array RoofNode -> Event UUID
getActivated ns = foldl (<|>) empty (f <$> ns)
    where f n = const (n ^. _roofId) <$> (n ^. _tapped)

createWrapper :: Effect Object3D
createWrapper = do
    wrapper <- mkObject3D
    setName "roof wrapper" wrapper
    pure wrapper

-- | function to create roof node
mkNode :: Event (Maybe UUID) -> PanelsDict -> Map Int OldRoofRackingData -> RoofNodeConfig -> RoofPlate -> HouseEditor RoofNode
mkNode activeRoof panelsDict racks cfg roof = runArrayBuilder rackTypeDyn roofNodeBuilder
    where roofNodeBuilder = createRoofNode $ cfg # _roof            .~ roof
                                                 # _roofActive      .~ roofActive
                                                 # _initPanels      .~ delay 100 (pure ps)

          rid = roof ^. _id
          ps = fromMaybe Nil (lookup rid panelsDict)
          rackType    = fromMaybe XR10 $ guessRackingType <$> lookup (roof ^. _roofIntId) racks
          rackTypeDyn = step rackType empty
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
                                      -> Event (Maybe UUID)
                                      -> Event RoofDictData
                                      -> PanelsDict
                                      -> Map Int OldRoofRackingData
                                      -> HouseEditor (Tuple (Event (Array RoofNode)) (Effect Unit))
renderRoofs wrapper activeRoof roofsData panelsDict racks = do
    { event: mainOrientE, push: pushMainOrient } <- liftEffect create

    let rsToRenderArr = dictToUnfoldable <$> compact (view _roofsToRender <$> roofsData)
        
        mainOrientDyn = step Landscape mainOrientE
        orientDyn     = step Landscape empty
        alignDyn      = step Grid empty
        opacityDyn    = step Opaque empty
        panelTypeDyn  = step def empty

        -- base config for roof node
        cfg = def # _mainOrientation .~ mainOrientDyn
                  # _orientation     .~ orientDyn
                  # _alignment       .~ alignDyn
                  # _panelType       .~ panelTypeDyn
                  # _opacity         .~ opacityDyn
    -- create roofnode for each roof and render them
    nodes <- performEditorEvent $ traverse (mkNode activeRoof panelsDict racks cfg) <$> rsToRenderArr
    let nodesEvt      = multicast $ performEvent $ renderNodes wrapper <$> withLast nodes
        mainOrientEvt = keepLatest $ calcMainOrientation <$> nodesEvt
    
    d <- liftEffect $ subscribe mainOrientEvt pushMainOrient

    pure $ Tuple nodesEvt d

isRoofEditing :: HouseEditor (Dynamic Boolean)
isRoofEditing = map ((==) RoofEditing) <<< view _modeDyn <$> ask

-- | function to add the roof recognizer and recognize new roofs
recognizeNewRoofs :: HouseMeshData -> Object3D -> Event RoofDict -> Dynamic (Maybe UUID) -> Dynamic Boolean -> Effect RoofRecognizer
recognizeNewRoofs meshData wrapper newRoofs activeRoof canEditRoofDyn = do
    let canShowRecognizer = (&&) <$> (isNothing <$> activeRoof) <*> canEditRoofDyn
    -- create the roof recognizer and add it to the roof wrapper object
    recognizer <- createRoofRecognizer (meshData ^. _wrapper)
                                       (dictToUnfoldable <$> newRoofs)
                                       (meshData ^. _mesh <<< _mouseMove)
                                       canShowRecognizer
    add recognizer wrapper
    pure recognizer

getActiveRoof :: HouseMeshData -> Event UUID -> Event RoofOperation -> Event RoofPlate -> Event (Maybe UUID)
getActiveRoof meshData activated deleteRoofOp addedNewRoof =
                (const Nothing <$> meshData ^. _mesh <<< _tapped) <|>
                (Just <$> activated) <|>
                (delay 1 $ const Nothing <$> deleteRoofOp) <|>
                (delay 1 $ Just <<< view _id <$> addedNewRoof)

-- | create RoofManager for an array of roofs
createRoofManager :: HouseMeshData -> Map Int OldRoofRackingData -> HouseEditor RoofManager
createRoofManager meshData racks = do
    wrapper <- liftEffect createWrapper

    -- create an event stream for the current active id
    { event : activeRoof, push : updateActive }    <- liftEffect create
    { event : roofsData,  push : updateRoofsData } <- liftEffect create

    let activeRoofDyn = step Nothing activeRoof

    -- get the default roof plates as a dict
    defRoofDict    <- roofDict <<< view _roofPlates <$> ask
    panelsDict     <- panelsDict <<< view _panels <$> ask
    canEditRoofDyn <- isRoofEditing

    -- render roofs dynamically
    Tuple renderedNodes d <- renderRoofs wrapper activeRoof roofsData panelsDict racks

    let deleteRoofOp  = multicast $ keepLatest $ getRoofDelete <$> renderedNodes
        updateRoofOp  = keepLatest $ getRoofUpdate             <$> renderedNodes
        activatedRoof = keepLatest $ getActivated              <$> renderedNodes

        -- event of new roofs that will be updated on any change and
        -- run the roof flatten algorithm whenever there's new roof change
        newRoofs  = multicast $ view _roofs <$> roofsData
        flattened = performEvent $ doFlatten meshData <$>  newRoofs

    -- recognize new roofs
    recognizer <- liftEffect $ recognizeNewRoofs meshData wrapper newRoofs activeRoofDyn canEditRoofDyn
    let addedNewRoof = recognizer ^. _addedNewRoof
        addRoofOp    = RoofOpCreate <$> addedNewRoof
        ops          = addRoofOp <|> deleteRoofOp <|> updateRoofOp
    
    -- manage all roofs and update it with user operations.
    let defRoofData = RoofDictData { roofs: defRoofDict, roofsToRender: Just defRoofDict }
        roofData = fold applyRoofOp ops defRoofData
    d1 <- liftEffect $ subscribe roofData updateRoofsData

    let newActRoof = getActiveRoof meshData activatedRoof deleteRoofOp addedNewRoof
    d2 <- liftEffect $ subscribe newActRoof updateActive
    
    -- set default values
    liftEffect do
        updateRoofsData defRoofData
        updateActive Nothing

    let getRoofEdited = map toRoofEdited <<< dictToUnfoldable

    -- skipe the first roof in teh editedRoofs event, because it's the default data
    pure $ RoofManager {
        wrapper     : wrapper,
        editedRoofs : multicast $ skip 1 $ debounce (Milliseconds 1000.0) $ getRoofEdited <$> newRoofs,
        disposable  : sequence_ [d, d1, d2, dispose recognizer]
    }
