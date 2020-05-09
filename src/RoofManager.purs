module Editor.RoofManager where

import Prelude hiding (add,degree)

import Algorithm.MeshFlatten (flattenRoofPlates)
import Control.Alt ((<|>))
import Control.Monad.Reader (ask)
import Control.Plus (empty)
import Data.Array (cons)
import Data.Compactable (compact)
import Data.Foldable (foldl, sequence_, traverse_)
import Data.Lens (Lens', view, (^.), (%~), (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (toUnfoldable)
import Data.Map (Map, delete, fromFoldable, insert, lookup, member, update, values)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Editor.Common.Lenses (_disposable, _geometry, _id, _mesh, _mouseMove, _roofId, _roofs, _slope, _tapped, _verticeTree, _wrapper)
import Editor.Disposable (class Disposable, dispose)
import Editor.EditorMode (EditorMode(..))
import Editor.House (HouseMeshData)
import Editor.RoofNode (RoofNode, _roofDelete, _roofObject, _roofUpdate, createRoofNode)
import Editor.RoofRecognizer (RoofRecognizer, _addedNewRoof, _marker, createRoofRecognizer)
import Editor.WebEditor (WebEditor, _modeDyn, _panels, _roofPlates, performEditorEvent)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, step)
import FRP.Event (Event, create, fold, keepLatest, subscribe, withLast)
import FRP.Event.Extra (debounce, delay, multicast, performEvent, skip)
import Math.Angle (degree)
import Model.Roof.Panel (Panel, _roofUUID)
import Model.Roof.RoofPlate (RoofEdited, RoofOperation(..), RoofPlate, isFlat, toRoofEdited)
import Three.Core.Object3D (Object3D, add, mkObject3D, remove, setName)

newtype RoofManager a = RoofManager {
    wrapper     :: Object3D a,
    editedRoofs :: Event (Array RoofEdited),
    disposable  :: Effect Unit
}

derive instance newtypeRoofManager :: Newtype (RoofManager a) _

instance disposableRoofManager :: Disposable (RoofManager a) where
    dispose r = r ^. _disposable

_editedRoofs :: forall a. Lens' (RoofManager a) (Event (Array RoofEdited))
_editedRoofs = _Newtype <<< prop (SProxy :: SProxy "editedRoofs")

type RoofDict = Map String RoofPlate

roofDict :: Array RoofPlate -> RoofDict
roofDict = fromFoldable <<< map f
    where f r = Tuple (r ^. _id) r

dictToArr :: RoofDict -> Array RoofPlate
dictToArr = toUnfoldable <<< values

type PanelsDict = Map String (Array Panel)

panelDict :: Array Panel -> PanelsDict
panelDict = foldl f Map.empty
    where f d p = if member (p ^. _roofUUID) d
                  then update (Just <<< cons p) (p ^. _roofUUID) d
                  else insert (p ^. _roofUUID) [p] d

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

doFlatten :: forall a. HouseMeshData a -> RoofDict -> Effect Unit
doFlatten meshData rd = flattenRoofPlates (meshData ^. _geometry) 
                                          (meshData ^. _verticeTree)
                                          (meshData ^. (_mesh <<< _mesh))
                                          (toUnfoldable $ values rd)

-- | get roofUpdate event from an array of roof nodes
getRoofUpdate :: forall a. Array (RoofNode a) -> Event RoofOperation
getRoofUpdate ns = foldl (<|>) empty (view _roofUpdate <$> ns)

-- | get roofDelete event from an array of roof nodes
getRoofDelete :: forall a. Array (RoofNode a) -> Event RoofOperation
getRoofDelete ns = foldl (<|>) empty (view _roofDelete <$> ns)

-- | get the activated roof id event from an array of roof nodes
getActivated :: forall a. Array (RoofNode a) -> Event String
getActivated ns = foldl (<|>) empty (f <$> ns)
    where f n = const (n ^. _roofId) <$> (n ^. _tapped)

createWrapper :: forall a. Effect (Object3D a)
createWrapper = do
    wrapper <- mkObject3D
    setName "roof wrapper" wrapper
    pure wrapper

-- | function to create roof node
mkNode :: forall a. Event (Maybe String) -> PanelsDict -> RoofPlate -> WebEditor (RoofNode a)
mkNode activeRoof panelsDict roof = createRoofNode roof ps (step false $ multicast $ (==) (Just (roof ^. _id)) <$> activeRoof)
    where ps = zeroSlope <$> fromMaybe [] (lookup (roof ^. _id) panelsDict)
          -- make sure slope value in panels on non-flat roof is set to zero
          zeroSlope p = if isFlat roof
                        then p
                        else p # _slope .~ degree 0.0

-- helper function to delete and dispose an old roof node
delOldNode :: forall a b. Object3D a -> RoofNode b -> Effect Unit
delOldNode wrapper rn = do
    dispose rn
    remove (rn ^. _roofObject) wrapper

-- delete old nodes and add new ones to the wrapper
renderNodes :: forall a b. Object3D a -> { last :: Maybe (Array (RoofNode b)), now :: Array (RoofNode b)} -> Effect (Array (RoofNode b))
renderNodes wrapper { last, now } = do
    traverse_ (delOldNode wrapper) $ fromMaybe [] last
    traverse_ (flip add wrapper <<< view _roofObject) now
    pure now

-- | render dynamic roofs
renderRoofs :: forall a b. Object3D a -> Event (Maybe String) -> Event RoofDictData -> PanelsDict -> WebEditor (Event (Array (RoofNode b)))
renderRoofs wrapper activeRoof roofsData panelsDict = do
    let rsToRender = compact $ view _roofsToRender <$> roofsData
        rsToRenderArr = dictToArr <$> rsToRender
        
    -- create roofnode for each roof and render them
    nodes <- performEditorEvent $ traverse (mkNode activeRoof panelsDict) <$> rsToRenderArr
    pure $ multicast $ performEvent $ renderNodes wrapper <$> withLast nodes

isRoofEditing :: WebEditor (Dynamic Boolean)
isRoofEditing = map ((==) RoofEditing) <<< view _modeDyn <$> ask

-- | function to add the roof recognizer and recognize new roofs
recognizeNewRoofs :: forall a b c. HouseMeshData a -> Object3D b -> Event RoofDict -> Dynamic (Maybe String) -> Dynamic Boolean -> Effect (RoofRecognizer c)
recognizeNewRoofs meshData wrapper newRoofs activeRoof canEditRoofDyn = do
    let canShowRecognizer = (&&) <$> (isNothing <$> activeRoof) <*> canEditRoofDyn
    -- create the roof recognizer and add it to the roof wrapper object
    recognizer <- createRoofRecognizer (meshData ^. _wrapper)
                                       (dictToArr <$> newRoofs)
                                       (meshData ^. _mesh <<< _mouseMove)
                                       canShowRecognizer
    add (recognizer ^. _marker) wrapper
    pure recognizer

getActiveRoof :: forall a. HouseMeshData a -> Event String -> Event RoofOperation -> Event RoofPlate -> Event (Maybe String)
getActiveRoof meshData activated deleteRoofOp addedNewRoof =
                (const Nothing <$> meshData ^. _mesh <<< _tapped) <|>
                (Just <$> activated) <|>
                (delay 1 $ const Nothing <$> deleteRoofOp) <|>
                (delay 1 $ Just <<< view _id <$> addedNewRoof)

-- | create RoofManager for an array of roofs
createRoofManager :: forall a b. HouseMeshData a -> WebEditor (RoofManager b)
createRoofManager meshData = do
    wrapper <- liftEffect createWrapper

    -- create an event stream for the current active id
    { event : activeRoof, push : updateActive }    <- liftEffect create
    { event : roofsData,  push : updateRoofsData } <- liftEffect create

    let activeRoofDyn = step Nothing activeRoof

    -- get the default roof plates as a dict
    defRoofDict    <- roofDict <<< view _roofPlates <$> ask
    panelsDict     <- panelDict <<< view _panels <$> ask
    canEditRoofDyn <- isRoofEditing

    -- render roofs dynamically
    renderedNodes <- renderRoofs wrapper activeRoof roofsData panelsDict

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

    let getRoofEdited = map toRoofEdited <<< dictToArr

    -- skipe the first roof in teh editedRoofs event, because it's the default data
    pure $ RoofManager {
        wrapper     : wrapper,
        editedRoofs : multicast $ skip 1 $ debounce (Milliseconds 1000.0) $ getRoofEdited <$> newRoofs,
        disposable  : sequence_ [d1, d2, dispose recognizer]
    }
