module Editor.RoofEditor where

import Prelude hiding (add)

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Plus (empty)
import Custom.Mesh (TappableMesh, mkTappableMesh)
import Data.Array (deleteAt, filter, foldl, head, insertAt, length, mapWithIndex, range, snoc, tail, take, takeEnd, zip, zipWith)
import Data.Compactable (compact)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence, sequence_, sum, traverse, traverse_)
import Data.Tuple (Tuple(..), fst, snd)
import Editor.SceneEvent (SceneTapEvent)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (Event, create, keepLatest, sampleOn, subscribe)
import Three.Core.Geometry (Geometry, mkCircleGeometry)
import Three.Core.Material (Material, mkMeshBasicMaterial)
import Three.Core.Object3D (Object3D, add, remove, setPosition, setVisible)
import Three.Math.Vector (Vector2, Vector3, dist, mkVec2, mkVec3, vecX, vecY)
import UI.DraggableObject (DraggableObject, createDraggableObject)
import Unsafe.Coerce (unsafeCoerce)
import Util (foldEffect, performEvent)

toVec2 :: Vector3 -> Vector2
toVec2 v = mkVec2 (vecX v) (vecY v)


-- | create red markers for vertices
mkRedMarkers :: forall a. Event Boolean
                       -> Event (Maybe Int)
                       -> Array Vector2
                       -> Effect (Array (DraggableObject a))
mkRedMarkers roofActive activeMarker ps = traverse mkMarker psIdx
    where psIdx = zip ps (range 0 (length ps - 1))
          mkMarker (Tuple pos idx) = do
              -- determine whether the current marker should be active or not
              let f act Nothing = act
                  f act (Just actIdx) = act && actIdx == idx
              
                  isActive = lift2 f roofActive activeMarker
              createDraggableObject isActive idx pos Nothing Nothing


-- | get red markers' active status event
getRedMarkerActiveStatus :: forall a. Event (Array (DraggableObject a)) -> Event (Maybe Int)
getRedMarkerActiveStatus ms = statusForDragging <|> statusForNewMarker
    where g idx m = (\d -> if d then Just idx else Nothing) <$> m.isDragging
          h objs = foldl (<|>) empty (mapWithIndex g objs)

          statusForDragging = keepLatest (h <$> ms)
          statusForNewMarker = const Nothing <$> ms

-- | delete old marker objects and add new ones.
attachObjs :: forall a b c. Object3D a -> Array (DraggableObject b) -> Array (DraggableObject c) -> Effect (Array (DraggableObject b))
attachObjs parent newObjs objs = do
    traverse_ (\o -> remove o.object parent *> o.disposable) objs
    traverse_ (\o -> add o.object parent) newObjs
    pure newObjs

roofDeleteMaterial :: forall a. Material a
roofDeleteMaterial = unsafeCoerce $ unsafePerformEffect (mkMeshBasicMaterial 0xffaa22)

roofDeleteGeometry :: forall a. Geometry a
roofDeleteGeometry = unsafeCoerce $ unsafePerformEffect (mkCircleGeometry 0.6 32)

-- | create the roof delete marker button
createRoofDeleteMarker :: forall a. Effect (TappableMesh a)
createRoofDeleteMarker = mkTappableMesh roofDeleteGeometry roofDeleteMaterial


-- | internal object for green marker point data
type GreenMarkerPoint = {
    position  :: Vector2,
    vertIndex :: Int
}

type GreenMarker a = {
    mesh  :: TappableMesh a,
    point :: GreenMarkerPoint
}

-- | create material and geometry for the green marker.
greenMaterial :: forall a. Material a
greenMaterial = unsafeCoerce $ unsafePerformEffect (mkMeshBasicMaterial 0x22ff22)

greenGeometry :: forall a. Geometry a
greenGeometry = unsafeCoerce $ unsafePerformEffect (mkCircleGeometry 0.3 32)

mkGreenMarkerMesh :: forall a. Vector2 -> Effect (TappableMesh a)
mkGreenMarkerMesh p = do
    m <- mkTappableMesh greenGeometry greenMaterial
    setPosition (mkVec3 (vecX p) (vecY p) 0.01) m.mesh
    pure m

mkGreenMarker :: forall a. GreenMarkerPoint -> Effect (GreenMarker a)
mkGreenMarker p = do
    m <- mkGreenMarkerMesh p.position
    pure { mesh: m, point: p }

-- | given a list of vertices position, calculate all middle points
greenMarkerPositions :: Array Vector2 -> Array GreenMarkerPoint
greenMarkerPositions [] = []
greenMarkerPositions [a] = []
greenMarkerPositions vertices = h <$> filter g d
    where -- take all vertices and their indices
          v1List = mapWithIndex (\i v -> Tuple i v) vertices
          -- a new list with the head put to end
          v2List = fromMaybe [] $ lift2 snoc (tail vertices) (head vertices)

          f :: Tuple Int Vector2 -> Vector2 -> { dist :: Number, point :: GreenMarkerPoint }
          f v v2 = let idx = fst v
                       v1 = snd v
                       point = { position: mkVec2 ((vecX v1 + vecX v2) / 2.0) ((vecY v1 + vecY v2) / 2.0),
                                 vertIndex: idx + 1
                               }
                    in { dist: dist v1 v2, point: point }

          d = zipWith f v1List v2List
          g r = r.dist > 1.0
          h r = r.point

setActive :: forall a. Array (GreenMarker a) -> Boolean -> Effect Unit
setActive ms active = traverse_ (\m -> setVisible active m.mesh.mesh) ms

updatePos :: forall c. GreenMarker c -> GreenMarkerPoint -> Effect (GreenMarker c)
updatePos o p = do
    setPosition (mkVec3 (vecX p.position) (vecY p.position) 0.01) o.mesh.mesh
    pure { mesh: o.mesh, point: p }

-- function to create/delete/update green marker objects based on new
-- list of GreenMarkerPoint
updateMarkers :: forall a b. Object3D a -> Array GreenMarkerPoint -> Array (GreenMarker b) -> Effect (Array (GreenMarker b))
updateMarkers parent ps oldObjs | length ps == length oldObjs = sequence (zipWith updatePos oldObjs ps)
                                | length ps > length oldObjs = do
                                        updObjs <- sequence (zipWith updatePos oldObjs (take (length oldObjs) ps))
                                        newObjs <- sequence (mkGreenMarker <$> takeEnd (length ps - length oldObjs) ps)
                                        traverse_ (\o -> add o.mesh.mesh parent) newObjs
                                        pure (updObjs <> newObjs)
                                | length ps < length oldObjs = do
                                        updObjs <- sequence (zipWith updatePos (take (length ps) oldObjs) ps)
                                        let delObjs = takeEnd (length oldObjs - length ps) oldObjs
                                        traverse_ (\o -> remove o.mesh.mesh parent) delObjs
                                        pure updObjs
                                | otherwise = pure oldObjs

mkGreenMarkers :: forall a. Object3D a -> Event Boolean -> Event (Array Vector2) -> Event GreenMarkerPoint
mkGreenMarkers parent active vertices = keepLatest $ getTapForAll <$> markers
    where mPosList = greenMarkerPositions <$> vertices
          markers = foldEffect (updateMarkers parent) mPosList []
          res = performEvent $ lift2 setActive markers active

          getTap m = const m.point <$> m.mesh.tapped
          getTapForAll ms = foldl (<|>) empty (getTap <$> ms)


-- | calculate the center based on roof vertices
verticesCenter :: Array Vector2 -> Vector3
verticesCenter [] = mkVec3 0.0 0.0 0.01
verticesCenter vs = mkVec3 (tx / l) (ty / l) 0.01
    where tx = sum (vecX <$> vs)
          ty = sum (vecY <$> vs)
          l = toNumber (length vs)


type RoofEditor = {
    roofVertices :: Event (Array Vector2),
    deleteRoof   :: Event SceneTapEvent,
    disposable   :: Effect Unit
}


-- get new positions after dragging
getPosition :: forall a. Array (DraggableObject a) -> Event (Array Vector2)
getPosition os = foldl (<>) empty (f <$> os)
    where f o = g <$> o.position
          g p = [toVec2 p]

getDelEvt :: forall a. Array (DraggableObject a) -> Event Int
getDelEvt os = foldl (<|>) empty (f <$> os)
    where f o = o.tapped

delMarker :: Array Vector2 -> Int -> Array Vector2
delMarker ps idx = fromMaybe [] (deleteAt idx ps)

-- | create roof editor
createRoofEditor :: forall a. Object3D a -> Event Boolean -> Array Vector2 -> Effect RoofEditor
createRoofEditor parent active ps = do
    -- internal event for maintaining the roof active status
    { event: roofActive, push: setRoofActive } <- create
    -- pipe the 'active' param event into internal roofActive event
    d1 <- subscribe active setRoofActive

    -- event for new list of vertices
    { event: vertices, push: updateVertList } <- create
    -- internal event for currently active marker
    { event: activeMarker, push: setActiveMarker } <- create

    -- create new markers and attach them to the parent object
    let markerObjs = performEvent $ mkRedMarkers roofActive activeMarker <$> vertices
        markers = foldEffect (attachObjs parent) markerObjs []

        -- event for active red marker
        actMarker = getRedMarkerActiveStatus markers
    d2 <- subscribe actMarker setActiveMarker

    -- get new positions after dragging
    let vertsAfterDrag = keepLatest $ getPosition <$> markers

        -- merge new vertices after dragging and vertices after adding/deleting
        newVertices = vertices <|> vertsAfterDrag
    
        greenActive = lift2 (\ra am -> ra && am == Nothing) roofActive activeMarker
        -- create green markers for adding new vertices
        toAddEvt = mkGreenMarkers parent greenActive newVertices
        addVert pns p = insertAt p.vertIndex p.position pns
        vertsAfterAdd = compact (sampleOn toAddEvt (addVert <$> newVertices))

        -- get delete event of tapping on a marker
        delEvts = keepLatest (getDelEvt <$> markers)
        -- calculate new vertices after deleting a vertex
        vertsAfterDel = lift2 delMarker newVertices delEvts
    
    -- update the real vertex list after adding/deleting
    d3 <- subscribe (vertsAfterAdd <|> vertsAfterDel) \vs -> do
            updateVertList vs
            setRoofActive true
    
    -- create the roof delete button
    roofDel <- createRoofDeleteMarker
    add roofDel.mesh parent

    -- update roof delete button position
    d4 <- subscribe (verticesCenter <$> newVertices) (flip setPosition roofDel.mesh)

    -- Show the roof delete button when roof's active
    d5 <- subscribe roofActive (flip setVisible roofDel.mesh)

    -- set the default vertices
    updateVertList ps
    -- disable roof by default
    setRoofActive false

    pure {
        roofVertices: newVertices,
        deleteRoof: roofDel.tapped,
        disposable: sequence_ [d1, d2, d3, d4, d5]
    }