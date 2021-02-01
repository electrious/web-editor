module Editor.HouseBuilder.GraphEditor where

import Prelude

import Algorithm.PointInPolygon (underPolygons)
import Control.Alt ((<|>))
import Control.Alternative (empty)
import Control.Apply (lift2)
import Custom.Mesh (TappableMesh)
import Data.Array (foldl)
import Data.Default (class Default, def)
import Data.Filterable (filter)
import Data.Foldable (class Foldable, find, null, traverse_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Graph (Graph, adjacent, vertices)
import Data.Lens (Lens', view, (^.), (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List, head)
import Data.Map (fromFoldable)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Meter (Meter, meterVal)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd)
import Data.UGraph (UGraph, addPolygon, deleteEdge, deleteVertex, edges, graphPoints, insertEdge, insertVertex, mergeVertices)
import Data.UGraph as UG
import Data.UUID (UUID, genUUID)
import Data.UUIDMap (UUIDMap)
import Editor.Common.Lenses (_active, _face, _floor, _mouseMove, _name, _point, _position)
import Editor.HeightEditor (dragArrowPos, setupHeightEditor)
import Editor.MarkerPoint (MidMarker, MidMarkerPoint(..), Modifier, VertMarker, VertMarkerPoint, _dragEndPos, _vert1, _vert2, getVertMarkerActiveStatus, getVertMarkerDragging, mkVertMarkerPoint)
import Editor.ObjectAdder (CandidatePoint, createObjectAdder, mkCandidatePoint)
import Editor.PolygonEditor (_vertModifier, getTapEvt)
import Editor.SceneEvent (SceneMouseMoveEvent, SceneTapEvent)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, current, dynEvent, gateDyn, latestEvt, sampleDyn, step)
import FRP.Event (Event, keepLatest)
import FRP.Event.Extra (multicast, performEvent, skip)
import Math.Line (_end, _start, lineCenter)
import Model.ActiveMode (ActiveMode(..), fromBoolean, isActive)
import Model.HouseBuilder.FloorPlan (FloorPlan, floorPlanHousePoints)
import Model.Polygon (Polygon, polygonAround)
import Model.UUID (class HasUUID, assignNewId, assignNewIds, idLens)
import Rendering.DynamicNode (dynamic_, renderDynamic, renderEvent)
import Rendering.Node (Node, _visible, fixNodeDWith, getParent, line, localEnv, node, tapMesh)
import Three.Core.Face3 (normal)
import Three.Core.Geometry (CircleGeometry, mkCircleGeometry)
import Three.Core.Material (LineBasicMaterial, MeshBasicMaterial, mkLineBasicMaterial, mkMeshBasicMaterial)
import Three.Core.Object3D (worldToLocal)
import Three.Math.Vector (class Vector, dist, getVector, mkVec3, updateVector, vecX, vecY)
import Util (latestAnyEvtWith)

-- a data type for functions to merge two graph vertices
newtype VertMerger v = VertMerger (v -> v -> Effect v)

derive instance newtypeVertMerger :: Newtype (VertMerger v) _
-- a default merger will return the second argument
instance defaultVertMerger :: Default (VertMerger v) where
    def = VertMerger (\v1 v2 -> pure v2)

mergeWith :: forall v.VertMerger v -> v -> v -> Effect v
mergeWith (VertMerger f) v1 v2 = f v1 v2

newtype GraphEditorConf v w = GraphEditorConf {
    active         :: Dynamic ActiveMode,
    floor          :: Dynamic FloorPlan,
    graph          :: Dynamic (UGraph v w),
    vertModifier   :: Modifier v,
    vertMerger     :: VertMerger v,
    heightEditable :: v -> Boolean,
    mouseMove      :: Event SceneMouseMoveEvent
    }

derive instance newtypeGraphEditorConf :: Newtype (GraphEditorConf v w) _
instance defaultGraphEditorConf :: Ord v => Default (GraphEditorConf v w) where
    def = GraphEditorConf {
        active         : pure Inactive,
        floor          : pure def,
        graph          : pure UG.empty,
        vertModifier   : def,
        vertMerger     : def,
        heightEditable : const true,
        mouseMove      : empty
        }

newtype GraphEditor v w = GraphEditor {
    graph      :: Event (UGraph v w),
    delete     :: Event SceneTapEvent,
    isDragging :: Event Boolean
    }

derive instance newtypeGraphEditor :: Newtype (GraphEditor v w) _

_graph :: forall t a r. Newtype t { graph :: a | r } => Lens' t a
_graph = _Newtype <<< prop (SProxy :: SProxy "graph")

_vertMerger :: forall t a r. Newtype t { vertMerger :: a | r } => Lens' t a
_vertMerger = _Newtype <<< prop (SProxy :: SProxy "vertMerger")

_heightEditable :: forall t a r. Newtype t { heightEditable :: a | r } => Lens' t a
_heightEditable = _Newtype <<< prop (SProxy :: SProxy "heightEditable")

-- create vertex markers for an array of vertices
mkVertMarkerPoints :: forall v w. HasUUID v => Modifier v -> Dynamic ActiveMode -> Dynamic (Maybe UUID) -> UGraph v w -> UUIDMap (VertMarkerPoint UUID v)
mkVertMarkerPoints m act actMarker graph = mapWithIndex (\i v -> mkVertMarkerPoint m act actMarker (Tuple v i)) ivsMap
    where ivsMap = fromFoldable $ f <$> vertices graph
          f v = Tuple (v ^. idLens) v

setupVertMarkers :: forall e v. Vector v => HasUUID v => Dynamic (UUIDMap (VertMarkerPoint UUID v)) -> Node e (Dynamic (UUIDMap (VertMarker UUID v)))
setupVertMarkers = renderDynamic


graphMidPoints :: forall v w. Ord v => HasUUID v => Vector v => Dynamic ActiveMode -> UGraph v w -> Effect (List (MidMarkerPoint UUID v))
graphMidPoints actDyn = traverse midP <<< edges
    where midP l = do
              i <- genUUID
              pos <- assignNewId $ lineCenter l
              pure $ MidMarkerPoint {
                  position : pos,
                  index    : pos ^. idLens,
                  vert1    : l ^. _start,
                  vert2    : l ^. _end,
                  active   : actDyn
                  }

-- | render all middle markers
setupMidMarkers :: forall e v w. Ord v => HasUUID v => Vector v => Dynamic ActiveMode -> Event (UGraph v w) -> Node e (Event (MidMarkerPoint UUID v))
setupMidMarkers actDyn graphEvt = do
    let mPointsEvt = performEvent $ graphMidPoints actDyn <$> graphEvt
    markers :: (Event (List (MidMarker UUID v))) <- renderEvent mPointsEvt
    pure $ keepLatest $ getTapEvt <$> markers


-----------------------------------------------------------
-- Marker to delete the current polygon
polyDelMat :: MeshBasicMaterial
polyDelMat = unsafePerformEffect (mkMeshBasicMaterial 0xffaa22)

polyDelGeo :: CircleGeometry
polyDelGeo = unsafePerformEffect (mkCircleGeometry 0.6 32)

-- | create the graph delete marker button
mkPolyDelMarker :: forall e v. Vector v => Event v -> Dynamic ActiveMode -> Node e TappableMesh
mkPolyDelMarker posEvt actDyn = tapMesh (def # _name     .~ "delete-marker"
                                             # _position .~ step def (getVector <$> posEvt)
                                             # _visible  .~ (isActive <$> actDyn)
                                        ) polyDelGeo polyDelMat


-----------------------------------------------------------
-- setup RoofSurface Adder to add new surface to the roof graph
validCandPoint :: forall v f. Default v =>
                              Vector v =>
                              Functor f =>
                              Foldable f =>
                              CandidatePoint v -> Polygon v -> f v -> Boolean
validCandPoint p poly pnts = underPolygons [poly] (p ^. _position) && isNothing (find f pnts)
    where f v = dist v (p ^. _position) < 2.0

setupPolyAdder :: forall e f v w. Functor f
                  => Foldable f
                  => Default v
                  => Vector v
                  => HasUUID v
                  => Dynamic (Polygon v) -> Dynamic (f v) -> GraphEditorConf v w -> Node e (Event (Polygon v))
setupPolyAdder polyDyn pntsDyn conf = do
    parent <- getParent
    
    let canShowAdder = isActive <$> conf ^. _active

        -- get a candidate point
        getCandPoint evt = do
            np <- worldToLocal (evt ^. _point) parent
            let c = updateVector def np
            pure $ mkCandidatePoint c (normal $ evt ^. _face)

        pntsEvt = performEvent $ getCandPoint <$> gateDyn canShowAdder (conf ^. _mouseMove)

        f p poly pnts = if validCandPoint p poly pnts then Just p else Nothing
        
        -- candidate point dynamic
        candPntDyn = step Nothing $ sampleDyn pntsDyn $ sampleDyn polyDyn $ f <$> pntsEvt

        opt = def # _name     .~ "poly-adder"
                  # _position .~ pure (mkVec3 0.0 0.0 0.1)

    addedPntEvt <- node opt $ createObjectAdder candPntDyn canShowAdder
    
    pure $ performEvent $ assignNewIds <<< polygonAround 3.0 <<< view _position <$> addedPntEvt


lineMat :: LineBasicMaterial
lineMat = unsafePerformEffect $ mkLineBasicMaterial 0x333333 2.0

-- render graph edges with lines
renderGraph :: forall e v w. Vector v => Ord v => UGraph v w -> Node e Unit
renderGraph = traverse_ renderLine <<< edges
    where renderLine l = do
              let vs = getVector <$> [l ^. _start, l ^. _end]
              line (def # _name .~ "graph-edge") vs lineMat

renderGraphDyn :: forall e v w. Vector v => Ord v => Dynamic ActiveMode -> Dynamic (UGraph v w) -> Node e Unit
renderGraphDyn active graphDyn = node (def # _name    .~ "graph-line"
                                           # _visible .~ (isActive <$> active)) $ dynamic_ $ renderGraph <$> graphDyn

-- update a vertex in a graph
updateVert :: forall v w. HasUUID v => Ord v => Default w => v -> UGraph v w -> UGraph v w
updateVert v g = foldl addEdge (insertVertex v $ deleteVertex v g) (adjacent v g)
    where addEdge g' v' = insertEdge v v' def g'

-- add a new vertex and edges based on clicked midmarker
addVert :: forall v w. Ord v => Default w => MidMarkerPoint UUID v -> UGraph v w -> UGraph v w
addVert mp g = let n = mp ^. _position
                   n1 = mp ^. _vert1
                   n2 = mp ^. _vert2
               in insertEdge n n1 def $ insertEdge n n2 def $ insertVertex n $ deleteEdge n1 n2 g

updateHeight :: forall v w. HasUUID v => Ord v => Vector v => Default w => (v -> Boolean) ->  Meter -> Graph v w -> Graph v w
updateHeight f h g = foldl upd g $ heightEditableVerts f g
    where upd g' v = updateVert (updateZ v) g'
          updateZ v = updateVector v $ setZ (meterVal h) (getVector v)
          setZ z v = mkVec3 (vecX v) (vecY v) z

checkAndMerge :: forall v w. Ord v => Vector v => HasUUID v => Default w => (v -> v -> Effect v) -> v -> Graph v w -> Effect (Graph v w)
checkAndMerge f v g = fromMaybe g <$> traverse merge cn
    where cn = head $ filter (\v' -> v' /= v && dist v v' < 2.0) $ vertices g
          merge vv = do
              nv <- f v vv
              pure $ mergeVertices v vv nv g

heightEditableVerts :: forall v w. (v -> Boolean) -> Graph v w -> List v
heightEditableVerts f = filter f <<< vertices

createGraphEditor :: forall e v w. Default v => Ord v => HasUUID v => Vector v => Default w => GraphEditorConf v w -> Node e (GraphEditor v w)
createGraphEditor cfg = do
    let active = cfg ^. _active
        merger = cfg ^. _vertMerger

    defAct <- liftEffect $ current active
    graph  <- liftEffect $ current $ cfg ^. _graph
    
    fixNodeDWith defAct \graphActive ->
        fixNodeDWith graph \noDragGraphDyn -> -- this is the graph only updated after add/delete vertices, dragging won't affect it
            fixNodeDWith graph \graphDyn ->   -- this is the graph updated with all operations applied
                fixNodeDWith Nothing \actMarkerDyn -> do
                    -- setup vertices markers
                    let vertMarkerPntsDyn = mkVertMarkerPoints (cfg ^. _vertModifier) graphActive actMarkerDyn <$> noDragGraphDyn
                    vertMarkersDyn <- setupVertMarkers vertMarkerPntsDyn

                    -- events for active vertex marker
                    let newActMarkerEvt = getVertMarkerActiveStatus vertMarkersDyn
                        dragEvt         = snd <$> latestAnyEvtWith (view _position) vertMarkersDyn
                        dragEndPosEvt   = latestAnyEvtWith (view _dragEndPos) vertMarkersDyn

                        -- apply the dragged new vertex to the graph
                        -- Node: this will be triggered after first render automatically
                        graphAfterDragEvt = sampleDyn graphDyn $ updateVert <$> dragEvt
                        graphAfterDragEndEvt = performEvent $ sampleDyn graphDyn $ checkAndMerge (mergeWith merger) <$> dragEndPosEvt

                        midActive = lift2 (\pa am -> pa && fromBoolean (am == Nothing)) graphActive actMarkerDyn

                        floorPolyDyn = floorPlanHousePoints <$> cfg ^. _floor

                        -- height editable vertices
                        hFilter = cfg ^. _heightEditable
                        hVertsDyn = heightEditableVerts hFilter <$> graphDyn
                        hEditorActDyn = (&&) <$> midActive <*> (fromBoolean <<< not <<< null <$> hVertsDyn)

                    -- render graph lines
                    renderGraphDyn active graphDyn

                    -- create mid markers for adding new vertices
                    toAddEvt <- setupMidMarkers midActive (dynEvent graphDyn)
                    -- setup the polygon adder
                    newPolyEvt <- setupPolyAdder floorPolyDyn (graphPoints <$> graphDyn) cfg

                    -- setup the height editor
                    heightEvt <- localEnv (const def) $ setupHeightEditor hEditorActDyn (dragArrowPos <$> hVertsDyn)

                    let graphAfterAdd = sampleDyn graphDyn $ addVert <$> toAddEvt
                        graphAfterAddPoly = sampleDyn graphDyn $ addPolygon <$> newPolyEvt
                    
                        graphAfterHeight = sampleDyn graphDyn $ updateHeight hFilter <$> heightEvt

                        -- get delete event of tapping on a marker
                        delEvts = map snd $ latestEvt $ getTapEvt <$> vertMarkersDyn
                        graphAfterDel = sampleDyn graphDyn $ deleteVertex <$> delEvts

                        -- update the real graph after adding/deleting vertex
                        graphEvt = multicast $ graphAfterAdd <|>
                                               graphAfterDel <|>
                                               graphAfterAddPoly <|>
                                               graphAfterDragEndEvt <|>
                                               graphAfterHeight <|>
                                               dynEvent (cfg ^. _graph)
                        -- new graph after all kinds of changes
                        allNewGraphEvt = multicast $ graphEvt <|> graphAfterDragEvt

                        graphActEvt = dynEvent active <|> (const Active <$> graphEvt)

                    -- create the graph delete button
                    --graphDel <- mkPolyDelMarker (graphCenter <$> allNewGraphEvt) graphActive
                
                    let editor = GraphEditor {
                            graph      : skip 1 allNewGraphEvt,
                            delete     : empty, --multicast $ graphDel ^. _tapped,
                            isDragging : multicast $ getVertMarkerDragging vertMarkersDyn
                            }
                    pure { input: newActMarkerEvt, output : { input: allNewGraphEvt, output: { input: graphEvt, output : { input: graphActEvt, output : editor }}}}
    
