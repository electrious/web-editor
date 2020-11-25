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
import Model.Polygon (Polygon(..))
import Editor.SceneEvent (SceneTapEvent)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, subscribeDyn)
import FRP.Event (Event, create, keepLatest, sampleOn, subscribe)
import FRP.Event.Extra (foldEffect, mergeArray, multicast, performEvent)
import Model.Hardware.PanelModel (_isActive)
import Three.Core.Geometry (CircleGeometry, Geometry, mkCircleGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial)
import Three.Core.Object3D (class IsObject3D, add, remove, setName, setPosition, setVisible, toObject3D)
import Three.Math.Vector (Vector2, Vector3, dist, mkVec2, mkVec3, toVec2, vecX, vecY)
import UI.DraggableObject (DraggableObject, createDraggableObject)

-----------------------------------------------------------
-- vertex marker
newtype VertMarker = VertMarker {
    position :: Vector2,
    index    :: Int,
    isActive :: Event Boolean
}

derive instance newtypeVertMarker :: Newtype VertMarker _

renderVertMarker :: VertMarker -> Effect DraggableObject
renderVertMarker m = do
    mesh <- createDraggableObject (m ^. _isActive)
                                  (m ^. _index)
                                  (m ^. _position)
                                  (Nothing :: Maybe Geometry)
                                  Nothing
    setName "vertex-marker" mesh
    pure mesh

-- create a vertex marker 
mkVertMarker :: Event Boolean -> Event (Maybe Int) -> Tuple Vector2 Int -> VertMarker
mkVertMarker polyActive actMarker (Tuple pos idx) = VertMarker {
                                                        position : pos,
                                                        index    : idx,
                                                        isActive : isActive
                                                    }
    where f act Nothing       = act
          f act (Just actIdx) = act && actIdx == idx

          isActive = multicast $ lift2 f polyActive actMarker

-- create vertex markers for an array of vertices
mkVertMarkers :: Event Boolean -> Event (Maybe Int) -> Polygon -> Array VertMarker
mkVertMarkers polyActive actMarker (Polygon ps) = mkVertMarker polyActive actMarker <$> zip ps (range 0 (length ps - 1))

-- | get vertex markers' active status event
getVertMarkerActiveStatus :: Event (Array DraggableObject) -> Event (Maybe Int)
getVertMarkerActiveStatus ms = statusForDragging <|> statusForNewMarker
    where g idx m = (\d -> if d then Just idx else Nothing) <$> m ^. _isDragging
          h objs = foldl (<|>) empty (mapWithIndex g objs)

          statusForDragging  = keepLatest $ h <$> ms
          statusForNewMarker = const Nothing <$> ms

-- | delete old marker objects and add new ones.
attachObjs :: forall a. IsObject3D a => a -> Array DraggableObject -> Array DraggableObject -> Effect (Array DraggableObject)
attachObjs parent newObjs objs = do
    traverse_ (\o -> remove o parent *> dispose o) objs
    traverse_ (flip add parent) newObjs
    pure newObjs


-- create new markers and attach them to the parent object
setupVertMarkers :: forall p. IsObject3D p => p -> Event Boolean -> Event (Maybe Int) -> Event Polygon -> Event (Array DraggableObject)
setupVertMarkers parent polyActive activeMarker polyEvt = multicast $ foldEffect (attachObjs parent) markerObjs []
    where vertMarkers = mkVertMarkers polyActive activeMarker    <$> polyEvt
          markerObjs  = performEvent $ traverse renderVertMarker <$> vertMarkers

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
-- | internal object for middle marker point data
newtype MidMarkerPoint = MidMarkerPoint {
    position :: Vector2,
    index    :: Int
}

derive instance newtypeMidMarkerPoint :: Newtype MidMarkerPoint _

newtype MidMarker = MidMarker {
    mesh  :: TappableMesh,
    point :: MidMarkerPoint
}

derive instance newtypeMidMarker :: Newtype MidMarker _
instance isObject3DMidMarker :: IsObject3D MidMarker where
    toObject3D = toObject3D <<< view _mesh

-- | create material and geometry for the middle marker.
midMaterial :: MeshBasicMaterial
midMaterial = unsafePerformEffect (mkMeshBasicMaterial 0x22ff22)

midGeometry :: CircleGeometry
midGeometry = unsafePerformEffect (mkCircleGeometry 0.3 32)

mkMidMarker :: MidMarkerPoint -> Effect MidMarker
mkMidMarker p = do
    m <- mkTappableMesh midGeometry midMaterial
    setName "mid-marker" m
    let pos = p ^. _position
    setPosition (mkVec3 (vecX pos) (vecY pos) 0.01) m
    pure $ MidMarker { mesh: m, point: p }

-- | given a list of vertices position, calculate all middle points
midMarkerPositions :: Polygon -> Array MidMarkerPoint
midMarkerPositions (Polygon []) = []
midMarkerPositions (Polygon [a]) = []
midMarkerPositions (Polygon vertices) = h <$> filter g d
    where -- take all vertices and their indices
          v1List = mapWithIndex Tuple vertices
          -- a new list with the head put to end
          v2List = fromMaybe [] $ lift2 snoc (tail vertices) (head vertices)

          f :: Tuple Int Vector2 -> Vector2 -> { dist :: Number, point :: MidMarkerPoint }
          f v v2 = let idx = fst v
                       v1 = snd v
                       point = MidMarkerPoint {
                                 position : mkVec2 ((vecX v1 + vecX v2) / 2.0) ((vecY v1 + vecY v2) / 2.0),
                                 index    : idx + 1
                               }
                    in { dist: dist v1 v2, point: point }

          d = zipWith f v1List v2List
          g r = r.dist > 1.0
          h r = r.point

setActive :: Array MidMarker -> Boolean -> Effect Unit
setActive ms active = traverse_ (\m -> setVisible active m) ms

updatePos :: MidMarker -> MidMarkerPoint -> Effect MidMarker
updatePos o p = do
    let pos = p ^. _position
    setPosition (mkVec3 (vecX pos) (vecY pos) 0.01) o
    pure $ MidMarker { mesh: o ^. _mesh, point: p }

-- function to create/delete/update mid marker objects based on new
-- list of MidMarkerPoint
updateMarkers :: forall a. IsObject3D a => a -> Array MidMarkerPoint -> Array MidMarker -> Effect (Array MidMarker)
updateMarkers parent ps oldObjs | length ps == length oldObjs = sequence (zipWith updatePos oldObjs ps)
                                | length ps > length oldObjs = do
                                        updObjs <- sequence (zipWith updatePos oldObjs (take (length oldObjs) ps))
                                        newObjs <- sequence (mkMidMarker <$> takeEnd (length ps - length oldObjs) ps)
                                        traverse_ (flip add parent) newObjs
                                        pure (updObjs <> newObjs)
                                | length ps < length oldObjs = do
                                        updObjs <- sequence (zipWith updatePos (take (length ps) oldObjs) ps)
                                        let delObjs = takeEnd (length oldObjs - length ps) oldObjs
                                        traverse_ (flip remove parent) delObjs
                                        pure updObjs
                                | otherwise = pure oldObjs

mkMidMarkers :: forall a. IsObject3D a => a -> Event Boolean -> Event Polygon -> Event MidMarkerPoint
mkMidMarkers parent active polyEvt = keepLatest $ getTapForAll <$> markers
    where mPosList = midMarkerPositions <$> polyEvt
          markers = multicast $ foldEffect (updateMarkers parent) mPosList []
          res = performEvent $ lift2 setActive markers active

          getTap m = const (m ^. _point) <$> (m ^. _mesh ^. _tapped)
          getTapForAll ms = foldl (<|>) empty (getTap <$> ms)


-- | calculate the center based on polygon
polyCenter :: Polygon -> Vector3
polyCenter (Polygon []) = mkVec3 0.0 0.0 0.01
polyCenter (Polygon vs) = mkVec3 (tx / l) (ty / l) 0.01
    where tx = sum (vecX <$> vs)
          ty = sum (vecY <$> vs)
          l = toNumber (length vs)


newtype PolyEditor = PolyEditor {
    polygon    :: Event Polygon,
    delete     :: Event SceneTapEvent,
    disposable :: Effect Unit
}

derive instance newtypePolyEditor :: Newtype PolyEditor _
instance disposablePolyEditor :: Disposable PolyEditor where
    dispose e = e ^. _disposable

_delete :: forall t a r. Newtype t { delete :: a | r } => Lens' t a
_delete = _Newtype <<< prop (SProxy :: SProxy "delete")

-- get new positions after dragging
getPosition :: Array DraggableObject -> Event Polygon
getPosition os = Polygon <$> mergeArray (f <$> os)
    where f o = g <$> o ^. _position
          g p = toVec2 p

getDelEvt :: Array DraggableObject -> Event Int
getDelEvt os = foldl (<|>) empty (f <$> os)
    where f o = o ^. _tapped

delMarker :: Int -> Polygon -> Polygon
delMarker idx (Polygon ps) = Polygon $ fromMaybe [] (deleteAt idx ps)

-- | create polygon editor
createPolyEditor :: forall a. IsObject3D a => a -> Dynamic Boolean -> Polygon -> Effect PolyEditor
createPolyEditor parent active poly = do
    -- internal event for maintaining the polygon active status
    { event: polyActive, push: setPolyActive } <- create
    -- pipe the 'active' param event into internal polyActive event
    d1 <- subscribeDyn active setPolyActive

    -- event for new list of vertices
    { event: polyEvt, push: updatePoly } <- create
    -- internal event for currently active marker
    { event: activeMarker, push: setActiveMarker } <- create

    let vertMarkers = setupVertMarkers parent polyActive activeMarker polyEvt
    -- event for active vertex marker
    d2 <- subscribe (getVertMarkerActiveStatus vertMarkers) setActiveMarker

    -- get new positions after dragging
    let vertsAfterDrag = keepLatest $ getPosition <$> vertMarkers

        -- merge new vertices after dragging and vertices after adding/deleting
        newPolyEvt = multicast $ polyEvt <|> vertsAfterDrag
    
        midActive = multicast $ lift2 (\pa am -> pa && am == Nothing) polyActive activeMarker
        -- create mid markers for adding new vertices
        toAddEvt = mkMidMarkers parent midActive newPolyEvt
        addVert p (Polygon pns) = Polygon <$> insertAt (p ^. _index) (p ^. _position) pns
        vertsAfterAdd = compact (sampleOn newPolyEvt $ addVert <$> toAddEvt)

        -- get delete event of tapping on a marker
        delEvts = keepLatest $ getDelEvt <$> vertMarkers
        -- calculate new vertices after deleting a vertex
        vertsAfterDel = sampleOn newPolyEvt (delMarker <$> delEvts)
    
    -- update the real vertex list after adding/deleting
    d3 <- subscribe (vertsAfterAdd <|> vertsAfterDel) \vs -> do
            updatePoly vs
            setPolyActive true
    
    -- create the polygon delete button
    polyDel <- mkPolyDelMarker
    add polyDel parent

    -- update polygon delete button position
    d4 <- subscribe (polyCenter <$> newPolyEvt) (flip setPosition polyDel)

    -- Show the polygon delete button when polygon's active
    d5 <- subscribe polyActive (flip setVisible polyDel)

    -- set the default vertices
    updatePoly poly
    -- disable polygon by default
    setPolyActive false

    pure $ PolyEditor {
        polygon    : newPolyEvt,
        delete     : polyDel ^. _tapped,
        disposable : sequence_ [d1, d2, d3, d4, d5]
    }