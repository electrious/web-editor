module Editor.RoofManager where

import Prelude hiding (add)

import Algorithm.MeshFlatten (flattenRoofPlates)
import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Compactable (compact)
import Data.Foldable (foldl, sequence_, traverse_)
import Data.List (toUnfoldable)
import Data.Map (Map, delete, fromFoldable, insert, values)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Editor.House (HouseMeshData)
import Editor.RoofNode (RoofNode, createRoofNode)
import Editor.RoofRecognizer (createRoofRecognizer)
import Effect (Effect)
import FRP.Event (Event, create, fold, keepLatest, subscribe, withLast)
import Models.RoofPlate (RoofEdited, RoofOperation(..), RoofPlate, toRoofEdited)
import Three.Core.Object3D (Object3D, add, mkObject3D, remove, setName)
import FRP.Event.Extra (debounce, delay, multicast, performEvent, skip)

type RoofManager a = {
    roofWrapper :: Object3D a,
    editedRoofs :: Event (Array RoofEdited),
    disposable  :: Effect Unit
}

type RoofDict = Map String RoofPlate

roofDict :: Array RoofPlate -> RoofDict
roofDict = fromFoldable <<< map f
    where f r = Tuple r.id r

-- internal data structure used to manage roofs
type RoofDictData = {
    roofs :: RoofDict,  -- all roofs manaaged, will be updated on any changes
    roofsToRender :: Maybe RoofDict  -- roofs used for rerenderring
}

-- | update the managed roof dict with new operation
updateRoofDict :: RoofOperation -> RoofDictData -> RoofDictData
updateRoofDict (RoofOpCreate roof) rd = let roofs = insert roof.id roof rd.roofs
                                      in { roofs: roofs, roofsToRender: Just roofs }
updateRoofDict (RoofOpDelete rid) rd = let roofs = delete rid rd.roofs
                                        in { roofs: roofs, roofsToRender: Just roofs }
updateRoofDict (RoofOpUpdate roof) rd = let roofs = insert roof.id roof rd.roofs
                                        in { roofs: roofs, roofsToRender: Nothing }

doFlatten :: forall a. HouseMeshData a -> RoofDict -> Effect Unit
doFlatten meshData rd = flattenRoofPlates meshData.geometry meshData.verticeTree meshData.mesh.mesh (toUnfoldable $ values rd)

-- | get roofUpdate event from an array of roof nodes
getRoofUpdate :: forall a. Array (RoofNode a) -> Event RoofOperation
getRoofUpdate ns = foldl (<|>) empty (_.roofUpdate <$> ns)

-- | get roofDelete event from an array of roof nodes
getRoofDelete :: forall a. Array (RoofNode a) -> Event RoofOperation
getRoofDelete ns = foldl (<|>) empty (_.roofDelete <$> ns)

-- | get the activated roof id event from an array of roof nodes
getActivated :: forall a. Array (RoofNode a) -> Event String
getActivated ns = foldl (<|>) empty (f <$> ns)
    where f n = const n.roofId <$> n.tapped

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
    let mkNode roof = createRoofNode roof (multicast $ ((==) (Just roof.id)) <$> activeRoof)
        defRoofDict = roofDict defRoofs

        -- get roofs to be rerendered
        rsToRender = compact $ _.roofsToRender <$> roofsData
        rsToRenderArr = (toUnfoldable <<< values) <$> rsToRender

        -- helper function to deleta and re-add roof nodes
        renderNodes { last, now } = do
            traverse_ (\o -> remove o.roofObject wrapper) $ fromMaybe [] last
            traverse_ (\o -> add o.roofObject wrapper) now
            pure now
        -- create roofnode for each roof
        nodes = performEvent $ (traverse mkNode <$> rsToRenderArr)
        renderedNodes = multicast $ performEvent $ renderNodes <$> withLast nodes

        deleteRoofOp = multicast $ keepLatest $ getRoofDelete <$> renderedNodes
        updateRoofOp = keepLatest $ getRoofUpdate <$> renderedNodes

        -- event of new roofs that will be updated on any change and
        -- run the roof flatten algorithm whenever there's new roof change
        newRoofs = multicast $ _.roofs <$> roofsData
        flattened = performEvent $ doFlatten meshData <$> newRoofs

        -- create the roof recognizer and add it to the roof wrapper object
        canShowRecognizer = isNothing <$> activeRoof

    recognizer <- createRoofRecognizer meshData.wrapper
                                       ((toUnfoldable <<< values) <$> newRoofs)
                                       meshData.mesh.mouseMove
                                       canShowRecognizer

    add recognizer.marker wrapper

    let addedNewRoof = recognizer.addedNewRoof
        addRoofOp = RoofOpCreate <$> addedNewRoof
        ops = addRoofOp <|> deleteRoofOp <|> updateRoofOp

    d2 <- subscribe (Just <$> (keepLatest $ getActivated <$> renderedNodes)) updateActive
    d3 <- subscribe (delay 1 $ const Nothing <$> deleteRoofOp) updateActive
    d4 <- subscribe (delay 1 $ (\o -> Just o.id) <$> addedNewRoof) updateActive

    -- manage all roofs and update it with user operations.
    let defRoofData = { roofs: defRoofDict, roofsToRender: Just defRoofDict }
        roofData = fold updateRoofDict ops defRoofData
    d5 <- subscribe roofData updateRoofsData

    updateRoofsData defRoofData

    updateActive Nothing

    let getRoofEdited = map toRoofEdited <<< toUnfoldable <<< values

    -- skipe the first roof in teh editedRoofs event, because it's the default data
    pure {
        roofWrapper: wrapper,
        editedRoofs: multicast $ skip 1 $ debounce (Milliseconds 1000.0) $ getRoofEdited <$> newRoofs,
        disposable: sequence_ [d1, d2, d3, d4, d5, recognizer.disposable]
    }
