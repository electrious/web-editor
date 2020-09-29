module Editor.RoofEditor where

import Prelude hiding (add)

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Plus (empty)
import Custom.Mesh (TappableMesh, mkTappableMesh)
import Data.Array (deleteAt, filter, foldl, head, insertAt, length, mapWithIndex, range, snoc, tail, take, takeEnd, zip, zipWith)
import Data.Compactable (compact)
import Data.Int (toNumber)
import Data.Lens (Lens', view, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence, sequence_, sum, traverse, traverse_)
import Data.Tuple (Tuple(..), fst, snd)
import Editor.Common.Lenses (_disposable, _index, _mesh, _point, _position, _tapped)
import Editor.Disposable (class Disposable, dispose)
import Editor.SceneEvent (SceneTapEvent)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, subscribeDyn)
import FRP.Event (Event, create, keepLatest, sampleOn, subscribe)
import FRP.Event.Extra (foldEffect, mergeArray, multicast, performEvent)
import Three.Core.Geometry (CircleGeometry, Geometry, mkCircleGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial)
import Three.Core.Object3D (class IsObject3D, add, remove, setName, setPosition, setVisible, toObject3D)
import Three.Math.Vector (Vector2, Vector3, dist, mkVec2, mkVec3, vecX, vecY)
import UI.DraggableObject (DraggableObject, _isDragging, createDraggableObject)
import Unsafe.Coerce (unsafeCoerce)

toVec2 :: Vector3 -> Vector2
toVec2 v = mkVec2 (vecX v) (vecY v)


-- | create red markers for vertices
mkRedMarkers :: Event Boolean
             -> Event (Maybe Int)
             -> Array Vector2
             -> Effect (Array DraggableObject)
mkRedMarkers roofActive activeMarker ps = traverse mkMarker psIdx
    where psIdx = zip ps (range 0 (length ps - 1))
          mkMarker (Tuple pos idx) = do
              -- determine whether the current marker should be active or not
              let f act Nothing = act
                  f act (Just actIdx) = act && actIdx == idx
              
                  isActive = multicast $ lift2 f roofActive activeMarker
              m <- createDraggableObject isActive idx pos (Nothing :: Maybe Geometry) Nothing
              setName "red-marker" m
              pure m


-- | get red markers' active status event
getRedMarkerActiveStatus :: Event (Array DraggableObject) -> Event (Maybe Int)
getRedMarkerActiveStatus ms = statusForDragging <|> statusForNewMarker
    where g idx m = (\d -> if d then Just idx else Nothing) <$> m ^. _isDragging
          h objs = foldl (<|>) empty (mapWithIndex g objs)

          statusForDragging = keepLatest (h <$> ms)
          statusForNewMarker = const Nothing <$> ms

-- | delete old marker objects and add new ones.
attachObjs :: forall a. IsObject3D a => a -> Array DraggableObject -> Array DraggableObject -> Effect (Array DraggableObject)
attachObjs parent newObjs objs = do
    traverse_ (\o -> remove o parent *> dispose o) objs
    traverse_ (flip add parent) newObjs
    pure newObjs

roofDeleteMaterial :: MeshBasicMaterial
roofDeleteMaterial = unsafeCoerce $ unsafePerformEffect (mkMeshBasicMaterial 0xffaa22)

roofDeleteGeometry :: CircleGeometry
roofDeleteGeometry = unsafeCoerce $ unsafePerformEffect (mkCircleGeometry 0.6 32)

-- | create the roof delete marker button
createRoofDeleteMarker :: Effect TappableMesh
createRoofDeleteMarker = do
    m <- mkTappableMesh roofDeleteGeometry roofDeleteMaterial
    setName "delete-marker" m
    pure m

-- | internal object for green marker point data
newtype GreenMarkerPoint = GreenMarkerPoint {
    position :: Vector2,
    index    :: Int
}

derive instance newtypeGreenMarkerPoint :: Newtype GreenMarkerPoint _

newtype GreenMarker = GreenMarker {
    mesh  :: TappableMesh,
    point :: GreenMarkerPoint
}

derive instance newtypeGreenMarker :: Newtype GreenMarker _
instance isObject3DGreenMarker :: IsObject3D GreenMarker where
    toObject3D = toObject3D <<< view _mesh

-- | create material and geometry for the green marker.
greenMaterial :: MeshBasicMaterial
greenMaterial = unsafeCoerce $ unsafePerformEffect (mkMeshBasicMaterial 0x22ff22)

greenGeometry :: CircleGeometry
greenGeometry = unsafeCoerce $ unsafePerformEffect (mkCircleGeometry 0.3 32)

mkGreenMarkerMesh :: Vector2 -> Effect TappableMesh
mkGreenMarkerMesh p = do
    m <- mkTappableMesh greenGeometry greenMaterial
    setName "green-marker" m
    setPosition (mkVec3 (vecX p) (vecY p) 0.01) m
    pure m

mkGreenMarker :: GreenMarkerPoint -> Effect GreenMarker
mkGreenMarker p = do
    m <- mkGreenMarkerMesh $ p ^. _position
    pure $ GreenMarker { mesh: m, point: p }

-- | given a list of vertices position, calculate all middle points
greenMarkerPositions :: Array Vector2 -> Array GreenMarkerPoint
greenMarkerPositions [] = []
greenMarkerPositions [a] = []
greenMarkerPositions vertices = h <$> filter g d
    where -- take all vertices and their indices
          v1List = mapWithIndex Tuple vertices
          -- a new list with the head put to end
          v2List = fromMaybe [] $ lift2 snoc (tail vertices) (head vertices)

          f :: Tuple Int Vector2 -> Vector2 -> { dist :: Number, point :: GreenMarkerPoint }
          f v v2 = let idx = fst v
                       v1 = snd v
                       point = GreenMarkerPoint {
                                 position : mkVec2 ((vecX v1 + vecX v2) / 2.0) ((vecY v1 + vecY v2) / 2.0),
                                 index    : idx + 1
                               }
                    in { dist: dist v1 v2, point: point }

          d = zipWith f v1List v2List
          g r = r.dist > 1.0
          h r = r.point

setActive :: Array GreenMarker -> Boolean -> Effect Unit
setActive ms active = traverse_ (\m -> setVisible active m) ms

updatePos :: GreenMarker -> GreenMarkerPoint -> Effect GreenMarker
updatePos o p = do
    let pos = p ^. _position
    setPosition (mkVec3 (vecX pos) (vecY pos) 0.01) o
    pure $ GreenMarker { mesh: o ^. _mesh, point: p }

-- function to create/delete/update green marker objects based on new
-- list of GreenMarkerPoint
updateMarkers :: forall a. IsObject3D a => a -> Array GreenMarkerPoint -> Array GreenMarker -> Effect (Array GreenMarker)
updateMarkers parent ps oldObjs | length ps == length oldObjs = sequence (zipWith updatePos oldObjs ps)
                                | length ps > length oldObjs = do
                                        updObjs <- sequence (zipWith updatePos oldObjs (take (length oldObjs) ps))
                                        newObjs <- sequence (mkGreenMarker <$> takeEnd (length ps - length oldObjs) ps)
                                        traverse_ (flip add parent) newObjs
                                        pure (updObjs <> newObjs)
                                | length ps < length oldObjs = do
                                        updObjs <- sequence (zipWith updatePos (take (length ps) oldObjs) ps)
                                        let delObjs = takeEnd (length oldObjs - length ps) oldObjs
                                        traverse_ (flip remove parent) delObjs
                                        pure updObjs
                                | otherwise = pure oldObjs

mkGreenMarkers :: forall a. IsObject3D a => a -> Event Boolean -> Event (Array Vector2) -> Event GreenMarkerPoint
mkGreenMarkers parent active vertices = keepLatest $ getTapForAll <$> markers
    where mPosList = greenMarkerPositions <$> vertices
          markers = multicast $ foldEffect (updateMarkers parent) mPosList []
          res = performEvent $ lift2 setActive markers active

          getTap m = const (m ^. _point) <$> (m ^. _mesh ^. _tapped)
          getTapForAll ms = foldl (<|>) empty (getTap <$> ms)


-- | calculate the center based on roof vertices
verticesCenter :: Array Vector2 -> Vector3
verticesCenter [] = mkVec3 0.0 0.0 0.01
verticesCenter vs = mkVec3 (tx / l) (ty / l) 0.01
    where tx = sum (vecX <$> vs)
          ty = sum (vecY <$> vs)
          l = toNumber (length vs)


newtype RoofEditor = RoofEditor {
    roofVertices :: Event (Array Vector2),
    deleteRoof   :: Event SceneTapEvent,
    disposable   :: Effect Unit
}

derive instance newtypeRoofEditor :: Newtype RoofEditor _
instance disposableRoofEditor :: Disposable RoofEditor where
    dispose e = e ^. _disposable

_roofVertices :: Lens' RoofEditor (Event (Array Vector2))
_roofVertices = _Newtype <<< prop (SProxy :: SProxy "roofVertices")

_deleteRoof :: Lens' RoofEditor (Event SceneTapEvent)
_deleteRoof = _Newtype <<< prop (SProxy :: SProxy "deleteRoof")

-- get new positions after dragging
getPosition :: Array DraggableObject -> Event (Array Vector2)
getPosition os = mergeArray (f <$> os)
    where f o = g <$> o ^. _position
          g p = toVec2 p

getDelEvt :: Array DraggableObject -> Event Int
getDelEvt os = foldl (<|>) empty (f <$> os)
    where f o = o ^. _tapped

delMarker :: Int -> Array Vector2 -> Array Vector2
delMarker idx ps = fromMaybe [] (deleteAt idx ps)

-- | create roof editor
createRoofEditor :: forall a. IsObject3D a => a -> Dynamic Boolean -> Array Vector2 -> Effect RoofEditor
createRoofEditor parent active ps = do
    -- internal event for maintaining the roof active status
    { event: roofActive, push: setRoofActive } <- create
    -- pipe the 'active' param event into internal roofActive event
    d1 <- subscribeDyn active setRoofActive

    -- event for new list of vertices
    { event: vertices, push: updateVertList } <- create
    -- internal event for currently active marker
    { event: activeMarker, push: setActiveMarker } <- create

    -- create new markers and attach them to the parent object
    let markerObjs = performEvent $ mkRedMarkers roofActive activeMarker <$> vertices
        markers = multicast $ foldEffect (attachObjs parent) markerObjs []

        -- event for active red marker
        actMarker = getRedMarkerActiveStatus markers
    d2 <- subscribe actMarker setActiveMarker

    -- get new positions after dragging
    let vertsAfterDrag = keepLatest $ getPosition <$> markers

        -- merge new vertices after dragging and vertices after adding/deleting
        newVertices = multicast $ vertices <|> vertsAfterDrag
    
        greenActive = multicast $ lift2 (\ra am -> ra && am == Nothing) roofActive activeMarker
        -- create green markers for adding new vertices
        toAddEvt = mkGreenMarkers parent greenActive newVertices
        addVert p pns = insertAt (p ^. _index) (p ^. _position) pns
        vertsAfterAdd = compact (sampleOn newVertices $ addVert <$> toAddEvt)

        -- get delete event of tapping on a marker
        delEvts = keepLatest (getDelEvt <$> markers)
        -- calculate new vertices after deleting a vertex
        vertsAfterDel = sampleOn newVertices (delMarker <$> delEvts)
    
    -- update the real vertex list after adding/deleting
    d3 <- subscribe (vertsAfterAdd <|> vertsAfterDel) \vs -> do
            updateVertList vs
            setRoofActive true
    
    -- create the roof delete button
    roofDel <- createRoofDeleteMarker
    add roofDel parent

    -- update roof delete button position
    d4 <- subscribe (verticesCenter <$> newVertices) (flip setPosition roofDel)

    -- Show the roof delete button when roof's active
    d5 <- subscribe roofActive (flip setVisible roofDel)

    -- set the default vertices
    updateVertList ps
    -- disable roof by default
    setRoofActive false

    pure $ RoofEditor {
        roofVertices : newVertices,
        deleteRoof   : roofDel ^. _tapped,
        disposable   : sequence_ [d1, d2, d3, d4, d5]
    }