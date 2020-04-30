module Editor.RoofManager where

import Prelude hiding (add)

import Algorithm.MeshFlatten (flattenRoofPlates)
import Control.Alt ((<|>))
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
import Editor.Disposable (class Disposable, dispose)
import Editor.House (HouseMeshData)
import Editor.RoofNode (RoofNode, _roofDelete, _roofObject, _roofUpdate, _tapped, createRoofNode)
import Editor.RoofNode as RN
import Editor.RoofRecognizer (_addedNewRoof, _marker, createRoofRecognizer)
import Effect (Effect)
import FRP.Event (Event, create, fold, keepLatest, subscribe, withLast)
import FRP.Event.Extra (debounce, delay, multicast, performEvent, skip)
import Model.Roof.RoofPlate (RoofEdited, RoofOperation(..), RoofPlate, _roofId, toRoofEdited)
import Three.Core.Object3D (Object3D, add, mkObject3D, remove, setName)

newtype RoofManager a = RoofManager {
    roofWrapper :: Object3D a,
    editedRoofs :: Event (Array RoofEdited),
    disposable  :: Effect Unit
}

derive instance newtypeRoofManager :: Newtype (RoofManager a) _

instance disposableRoofManager :: Disposable (RoofManager a) where
    dispose r = r ^. _disposable

_roofWrapper :: forall a. Lens' (RoofManager a) (Object3D a)
_roofWrapper = _Newtype <<< prop (SProxy :: SProxy "roofWrapper")

_editedRoofs :: forall a. Lens' (RoofManager a) (Event (Array RoofEdited))
_editedRoofs = _Newtype <<< prop (SProxy :: SProxy "editedRoofs")

_disposable :: forall a. Lens' (RoofManager a) (Effect Unit)
_disposable = _Newtype <<< prop (SProxy :: SProxy "disposable")

type RoofDict = Map String RoofPlate

roofDict :: Array RoofPlate -> RoofDict
roofDict = fromFoldable <<< map f
    where f r = Tuple (r ^. _roofId) r

-- internal data structure used to manage roofs
newtype RoofDictData = RoofDictData {
    roofs :: RoofDict,  -- all roofs manaaged, will be updated on any changes
    roofsToRender :: Maybe RoofDict  -- roofs used for rerenderring
}

derive instance newtypeRoofDictData :: Newtype RoofDictData _

_roofs :: Lens' RoofDictData RoofDict
_roofs = _Newtype <<< prop (SProxy :: SProxy "roofs")

_roofsToRender :: Lens' RoofDictData (Maybe RoofDict)
_roofsToRender = _Newtype <<< prop (SProxy :: SProxy "roofsToRender")

-- | update the managed roof dict with new operation
updateRoofDict :: RoofOperation -> RoofDictData -> RoofDictData
updateRoofDict (RoofOpCreate roof) rd = let roofs = insert (roof ^. _roofId) roof (rd ^. _roofs)
                                      in RoofDictData { roofs: roofs, roofsToRender: Just roofs }
updateRoofDict (RoofOpDelete rid) rd = let roofs = delete rid $ rd ^. _roofs
                                        in RoofDictData { roofs: roofs, roofsToRender: Just roofs }
updateRoofDict (RoofOpUpdate roof) rd = let roofs = insert (roof ^. _roofId) roof (rd ^. _roofs)
                                        in RoofDictData { roofs: roofs, roofsToRender: Nothing }

doFlatten :: forall a. HouseMeshData a -> RoofDict -> Effect Unit
doFlatten meshData rd = flattenRoofPlates meshData.geometry meshData.verticeTree meshData.mesh.mesh (toUnfoldable $ values rd)

-- | get roofUpdate event from an array of roof nodes
getRoofUpdate :: forall a. Array (RoofNode a) -> Event RoofOperation
getRoofUpdate ns = foldl (<|>) empty (view _roofUpdate <$> ns)

-- | get roofDelete event from an array of roof nodes
getRoofDelete :: forall a. Array (RoofNode a) -> Event RoofOperation
getRoofDelete ns = foldl (<|>) empty (view _roofDelete <$> ns)

-- | get the activated roof id event from an array of roof nodes
getActivated :: forall a. Array (RoofNode a) -> Event String
getActivated ns = foldl (<|>) empty (f <$> ns)
    where f n = const (n ^. RN._roofId) <$> (n ^. _tapped)

-- | create RoofManager for an array of roofs
createRoofManager :: forall a b. HouseMeshData a -> Array RoofPlate -> Effect (RoofManager b)
createRoofManager meshData defRoofs = do
    wrapper <- mkObject3D
    setName "roof wrapper" wrapper

    -- create an event stream for the current active id
    { event: activeRoof, push: updateActive } <- create

    -- if house mesh is tapped, to deactivate all roofs
    d1 <- subscribe (const Nothing <$> meshData.mesh.tapped) updateActive

    { event: roofsData, push: updateRoofsData } <- create
    let mkNode roof = createRoofNode roof (multicast $ ((==) (Just $ roof ^. _roofId)) <$> activeRoof)
        defRoofDict = roofDict defRoofs

        -- get roofs to be rerendered
        rsToRender = compact $ view _roofsToRender <$> roofsData
        rsToRenderArr = (toUnfoldable <<< values) <$> rsToRender

        -- helper function to deleta and re-add roof nodes
        renderNodes { last, now } = do
            traverse_ (\o -> remove (o ^. _roofObject) wrapper) $ fromMaybe [] last
            traverse_ (\o -> add (o ^. _roofObject) wrapper) now
            pure now
        -- create roofnode for each roof
        nodes = performEvent $ (traverse mkNode <$> rsToRenderArr)
        renderedNodes = multicast $ performEvent $ renderNodes <$> withLast nodes

        deleteRoofOp = multicast $ keepLatest $ getRoofDelete <$> renderedNodes
        updateRoofOp = keepLatest $ getRoofUpdate <$> renderedNodes

        -- event of new roofs that will be updated on any change and
        -- run the roof flatten algorithm whenever there's new roof change
        newRoofs = multicast $ view _roofs <$> roofsData
        flattened = performEvent $ doFlatten meshData <$> newRoofs

        -- create the roof recognizer and add it to the roof wrapper object
        canShowRecognizer = isNothing <$> activeRoof

    recognizer <- createRoofRecognizer meshData.wrapper
                                       ((toUnfoldable <<< values) <$> newRoofs)
                                       meshData.mesh.mouseMove
                                       canShowRecognizer

    add (recognizer ^. _marker) wrapper

    let addedNewRoof = recognizer ^. _addedNewRoof
        addRoofOp = RoofOpCreate <$> addedNewRoof
        ops = addRoofOp <|> deleteRoofOp <|> updateRoofOp

    d2 <- subscribe (Just <$> (keepLatest $ getActivated <$> renderedNodes)) updateActive
    d3 <- subscribe (delay 1 $ const Nothing <$> deleteRoofOp) updateActive
    d4 <- subscribe (delay 1 $ (\o -> Just $ o ^. _roofId) <$> addedNewRoof) updateActive

    -- manage all roofs and update it with user operations.
    let defRoofData = RoofDictData { roofs: defRoofDict, roofsToRender: Just defRoofDict }
        roofData = fold updateRoofDict ops defRoofData
    d5 <- subscribe roofData updateRoofsData

    updateRoofsData defRoofData

    updateActive Nothing

    let getRoofEdited = map toRoofEdited <<< toUnfoldable <<< values

    -- skipe the first roof in teh editedRoofs event, because it's the default data
    pure $ RoofManager {
        roofWrapper: wrapper,
        editedRoofs: multicast $ skip 1 $ debounce (Milliseconds 1000.0) $ getRoofEdited <$> newRoofs,
        disposable: sequence_ [d1, d2, d3, d4, d5, dispose recognizer]
    }
