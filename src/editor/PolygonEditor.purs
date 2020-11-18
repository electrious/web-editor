module Editor.PolygonEditor where

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
import Editor.Common.Lenses (_disposable, _index, _isDragging, _mesh, _point, _position, _tapped)
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
import Three.Math.Vector (Vector2, Vector3, dist, mkVec2, mkVec3, toVec2, vecX, vecY)
import UI.DraggableObject (DraggableObject, createDraggableObject)

-----------------------------------------------------------
-- Red markers for vertices
type RedMarker = DraggableObject

-- create a red marker 
mkRedMarker :: Event Boolean -> Event (Maybe Int) -> Tuple Vector2 Int -> Effect RedMarker
mkRedMarker polyActive actMarker (Tuple pos idx) = do
    let f act Nothing       = act
        f act (Just actIdx) = act && actIdx == idx

        isActive = multicast $ lift2 f polyActive actMarker
    
    m <- createDraggableObject isActive idx pos (Nothing :: Maybe Geometry) Nothing
    setName "red-marker" m
    pure m


-- create red markers for an array of vertices
mkRedMarkers :: Event Boolean -> Event (Maybe Int) -> Array Vector2 -> Effect (Array RedMarker)
mkRedMarkers polyActive actMarker ps = traverse (mkRedMarker polyActive actMarker) $ zip ps (range 0 (length ps - 1))

-- | get red markers' active status event
getRedMarkerActiveStatus :: Event (Array RedMarker) -> Event (Maybe Int)
getRedMarkerActiveStatus ms = statusForDragging <|> statusForNewMarker
    where g idx m = (\d -> if d then Just idx else Nothing) <$> m ^. _isDragging
          h objs = foldl (<|>) empty (mapWithIndex g objs)

          statusForDragging = keepLatest (h <$> ms)
          statusForNewMarker = const Nothing <$> ms

-- | delete old marker objects and add new ones.
attachObjs :: forall a. IsObject3D a => a -> Array RedMarker -> Array RedMarker -> Effect (Array RedMarker)
attachObjs parent newObjs objs = do
    traverse_ (\o -> remove o parent *> dispose o) objs
    traverse_ (flip add parent) newObjs
    pure newObjs


-----------------------------------------------------------
-- Marker to delete the current polygon
polyDelMat :: MeshBasicMaterial
polyDelMat = unsafePerformEffect (mkMeshBasicMaterial 0xffaa22)

polyDelGeo :: CircleGeometry
polyDelGeo = unsafePerformEffect (mkCircleGeometry 0.6 32)

-- | create the polygon delete marker button
mkPolyDelMarker :: Effect TappableMesh
mkPolyDelMarker = do
    m <- mkTappableMesh polyDelGeo polyDelMat
    setName "delete-marker" m
    pure m


-----------------------------------------------------------
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
greenMaterial = unsafePerformEffect (mkMeshBasicMaterial 0x22ff22)

greenGeometry :: CircleGeometry
greenGeometry = unsafePerformEffect (mkCircleGeometry 0.3 32)

mkGreenMarker :: GreenMarkerPoint -> Effect GreenMarker
mkGreenMarker p = do
    m <- mkTappableMesh greenGeometry greenMaterial
    setName "green-marker" m
    let pos = p ^. _position
    setPosition (mkVec3 (vecX pos) (vecY pos) 0.01) m
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


-- | calculate the center based on polygon vertices
verticesCenter :: Array Vector2 -> Vector3
verticesCenter [] = mkVec3 0.0 0.0 0.01
verticesCenter vs = mkVec3 (tx / l) (ty / l) 0.01
    where tx = sum (vecX <$> vs)
          ty = sum (vecY <$> vs)
          l = toNumber (length vs)


newtype PolyEditor = PolyEditor {
    vertices   :: Event (Array Vector2),
    delete     :: Event SceneTapEvent,
    disposable :: Effect Unit
}

derive instance newtypePolyEditor :: Newtype PolyEditor _
instance disposablePolyEditor :: Disposable PolyEditor where
    dispose e = e ^. _disposable

_vertices :: forall t a r. Newtype t { vertices :: a | r } => Lens' t a
_vertices = _Newtype <<< prop (SProxy :: SProxy "vertices")

_delete :: forall t a r. Newtype t { delete :: a | r } => Lens' t a
_delete = _Newtype <<< prop (SProxy :: SProxy "delete")

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

-- | create polygon editor
createPolyEditor :: forall a. IsObject3D a => a -> Dynamic Boolean -> Array Vector2 -> Effect PolyEditor
createPolyEditor parent active ps = do
    -- internal event for maintaining the polygon active status
    { event: polyActive, push: setPolyActive } <- create
    -- pipe the 'active' param event into internal polyActive event
    d1 <- subscribeDyn active setPolyActive

    -- event for new list of vertices
    { event: vertices, push: updateVertList } <- create
    -- internal event for currently active marker
    { event: activeMarker, push: setActiveMarker } <- create

    -- create new markers and attach them to the parent object
    let markerObjs = performEvent $ mkRedMarkers polyActive activeMarker <$> vertices
        markers = multicast $ foldEffect (attachObjs parent) markerObjs []

        -- event for active red marker
        actMarker = getRedMarkerActiveStatus markers
    d2 <- subscribe actMarker setActiveMarker

    -- get new positions after dragging
    let vertsAfterDrag = keepLatest $ getPosition <$> markers

        -- merge new vertices after dragging and vertices after adding/deleting
        newVertices = multicast $ vertices <|> vertsAfterDrag
    
        greenActive = multicast $ lift2 (\ra am -> ra && am == Nothing) polyActive activeMarker
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
            setPolyActive true
    
    -- create the polygon delete button
    polyDel <- mkPolyDelMarker
    add polyDel parent

    -- update polygon delete button position
    d4 <- subscribe (verticesCenter <$> newVertices) (flip setPosition polyDel)

    -- Show the polygon delete button when polygon's active
    d5 <- subscribe polyActive (flip setVisible polyDel)

    -- set the default vertices
    updateVertList ps
    -- disable polygon by default
    setPolyActive false

    pure $ PolyEditor {
        vertices   : newVertices,
        delete     : polyDel ^. _tapped,
        disposable : sequence_ [d1, d2, d3, d4, d5]
    }