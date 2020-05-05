module Editor.RoofManager where

import Prelude hiding (add)

import Algorithm.MeshFlatten (flattenRoofPlates)
import Control.Alt ((<|>))
import Control.Monad.Reader (ask)
import Control.Plus (empty)
import Data.Compactable (compact)
import Data.Foldable (foldl, sequence_, traverse_)
import Data.Lens (Lens', view, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (toUnfoldable)
import Data.Map (Map, delete, fromFoldable, insert, values)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Editor.Common.Lenses (_disposable, _geometry, _id, _mesh, _mouseMove, _roofId, _roofs, _tapped, _verticeTree, _wrapper)
import Editor.Disposable (class Disposable, dispose)
import Editor.EditorMode (EditorMode(..))
import Editor.House (HouseMeshData)
import Editor.RoofNode (RoofNode, _roofDelete, _roofObject, _roofUpdate, createRoofNode)
import Editor.RoofRecognizer (_addedNewRoof, _marker, createRoofRecognizer)
import Editor.WebEditor (WebEditor, _defMode, _modeEvt, _roofPlates)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Event (Event, create, fold, keepLatest, subscribe, withLast)
import FRP.Event.Extra (after, debounce, delay, multicast, performEvent, skip)
import Model.Roof.RoofPlate (RoofEdited, RoofOperation(..), RoofPlate, toRoofEdited)
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

-- internal data structure used to manage roofs
newtype RoofDictData = RoofDictData {
    roofs :: RoofDict,  -- all roofs manaaged, will be updated on any changes
    roofsToRender :: Maybe RoofDict  -- roofs used for rerenderring
}

derive instance newtypeRoofDictData :: Newtype RoofDictData _

_roofsToRender :: Lens' RoofDictData (Maybe RoofDict)
_roofsToRender = _Newtype <<< prop (SProxy :: SProxy "roofsToRender")

-- | update the managed roof dict with new operation
updateRoofDict :: RoofOperation -> RoofDictData -> RoofDictData
updateRoofDict (RoofOpCreate roof) rd = let roofs = insert (roof ^. _id) roof (rd ^. _roofs)
                                      in RoofDictData { roofs: roofs, roofsToRender: Just roofs }
updateRoofDict (RoofOpDelete rid) rd = let roofs = delete rid $ rd ^. _roofs
                                        in RoofDictData { roofs: roofs, roofsToRender: Just roofs }
updateRoofDict (RoofOpUpdate roof) rd = let roofs = insert (roof ^. _id) roof (rd ^. _roofs)
                                        in RoofDictData { roofs: roofs, roofsToRender: Nothing }

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

-- | create RoofManager for an array of roofs
createRoofManager :: forall a b. HouseMeshData a -> WebEditor (RoofManager b)
createRoofManager meshData = do
    wrapper <- liftEffect mkObject3D
    liftEffect $ setName "roof wrapper" wrapper

    -- create an event stream for the current active id
    { event: activeRoof, push: updateActive } <- liftEffect create

    -- get the default roof plates as a dict
    defRoofDict <- roofDict <<< view _roofPlates <$> ask
    defMode <- view _defMode <$> ask
    modeEvt <- view _modeEvt <$> ask

    -- if house mesh is tapped, to deactivate all roofs
    d1 <- liftEffect $ subscribe (const Nothing <$> meshData ^. _mesh <<< _tapped) updateActive

    { event: roofsData, push: updateRoofsData } <- liftEffect create
    let mkNode roof = createRoofNode roof (multicast $ (==) (Just (roof ^. _id)) <$> activeRoof)

        -- get roofs to be rerendered
        rsToRender = compact $ view _roofsToRender <$> roofsData
        rsToRenderArr = toUnfoldable <<< values <$> rsToRender

        -- helper function to deleta and re-add roof nodes
        renderNodes { last, now } = do
            traverse_ (flip remove wrapper <<< view _roofObject) $ fromMaybe [] last
            traverse_ (flip add wrapper <<< view _roofObject) now
            pure now
        -- create roofnode for each roof
        nodes = performEvent $ (traverse mkNode <$> rsToRenderArr)
        renderedNodes = multicast $ performEvent $ renderNodes <$> withLast nodes

        deleteRoofOp = multicast $ keepLatest $ getRoofDelete <$> renderedNodes
        updateRoofOp = keepLatest $ getRoofUpdate <$> renderedNodes

        -- event of new roofs that will be updated on any change and
        -- run the roof flatten algorithm whenever there's new roof change
        newRoofs          = multicast $ view _roofs <$> roofsData
        flattened         = performEvent $ doFlatten meshData <$> newRoofs

        realModeEvt       = modeEvt <|> (const defMode <$> after 10)
        canEditRoofEvt    = (==) RoofEditing <$> realModeEvt
        canShowRecognizer = (&&) <$> (isNothing <$> activeRoof) <*> canEditRoofEvt
    
    -- create the roof recognizer and add it to the roof wrapper object
    recognizer <- liftEffect $ createRoofRecognizer (meshData ^. _wrapper)
                                                    (toUnfoldable <<< values <$> newRoofs)
                                                    (meshData ^. _mesh <<< _mouseMove)
                                                    canShowRecognizer

    liftEffect $ add (recognizer ^. _marker) wrapper

    let addedNewRoof = recognizer ^. _addedNewRoof
        addRoofOp    = RoofOpCreate <$> addedNewRoof
        ops          = addRoofOp <|> deleteRoofOp <|> updateRoofOp

    d2 <- liftEffect $ subscribe (Just <$> (keepLatest $ getActivated <$> renderedNodes)) updateActive
    d3 <- liftEffect $ subscribe (delay 1 $ const Nothing <$> deleteRoofOp) updateActive
    d4 <- liftEffect $ subscribe (delay 1 $ Just <<< view _id <$> addedNewRoof) updateActive

    -- manage all roofs and update it with user operations.
    let defRoofData = RoofDictData { roofs: defRoofDict, roofsToRender: Just defRoofDict }
        roofData = fold updateRoofDict ops defRoofData
    d5 <- liftEffect $ subscribe roofData updateRoofsData

    liftEffect $ updateRoofsData defRoofData

    liftEffect $ updateActive Nothing

    let getRoofEdited = map toRoofEdited <<< toUnfoldable <<< values

    -- skipe the first roof in teh editedRoofs event, because it's the default data
    pure $ RoofManager {
        wrapper     : wrapper,
        editedRoofs : multicast $ skip 1 $ debounce (Milliseconds 1000.0) $ getRoofEdited <$> newRoofs,
        disposable  : sequence_ [d1, d2, d3, d4, d5, dispose recognizer]
    }
