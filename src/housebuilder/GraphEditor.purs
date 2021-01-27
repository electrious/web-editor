module Editor.HouseBuilder.GraphEditor where

import Prelude

import Algorithm.PointInPolygon (underPolygons)
import Control.Alt ((<|>))
import Control.Alternative (empty)
import Control.Apply (lift2)
import Custom.Mesh (TappableMesh)
import Data.Array (foldl)
import Data.Default (class Default, def)
import Data.Foldable (class Foldable, find)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Graph (Graph, adjacent, deleteEdge, deleteVertex, insertEdge, insertVertex, vertices)
import Data.Graph as G
import Data.Graph.Extra (addPolygon, edges, graphCenter, graphPoints)
import Data.Lens (Lens', view, (^.), (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.Map (fromFoldable)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd)
import Data.UUID (UUID, genUUID)
import Data.UUIDMap (UUIDMap)
import Editor.Common.Lenses (_active, _face, _floor, _mouseMove, _name, _point, _position, _tapped)
import Editor.MarkerPoint (MidMarker, MidMarkerPoint(..), Modifier, VertMarker, VertMarkerPoint, _vert1, _vert2, getVertMarkerActiveStatus, getVertMarkerDragging, mkVertMarkerPoint)
import Editor.ObjectAdder (CandidatePoint, createObjectAdder, mkCandidatePoint)
import Editor.PolygonEditor (_vertModifier, getTapEvt)
import Editor.SceneEvent (SceneMouseMoveEvent, SceneTapEvent)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, current, dynEvent, gateDyn, latestEvt, sampleDyn, step)
import FRP.Event (Event, keepLatest, sampleOn)
import FRP.Event.Extra (anyEvt, multicast, performEvent, skip)
import Math.Line (_end, _start, lineCenter)
import Model.ActiveMode (ActiveMode(..), fromBoolean, isActive)
import Model.HouseBuilder.FloorPlan (FloorPlan, floorPlanHousePoints)
import Model.Polygon (Polygon, polygonAround)
import Model.UUID (class HasUUID, idLens)
import Rendering.DynamicNode (renderDynamic, renderEvent)
import Rendering.Node (Node, _visible, fixNodeDWith, getParent, node, tapMesh)
import Three.Core.Face3 (normal)
import Three.Core.Geometry (CircleGeometry, mkCircleGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial)
import Three.Core.Object3D (worldToLocal)
import Three.Math.Vector (class Vector, dist, getVector, mkVec3, updateVector)


newtype GraphEditorConf v w = GraphEditorConf {
    active       :: Dynamic ActiveMode,
    floor        :: Dynamic FloorPlan,
    graph        :: Graph v w,
    vertModifier :: Modifier v,
    mouseMove    :: Event SceneMouseMoveEvent
    }

derive instance newtypeGraphEditorConf :: Newtype (GraphEditorConf v w) _
instance defaultGraphEditorConf :: Ord v => Default (GraphEditorConf v w) where
    def = GraphEditorConf {
        active       : pure Inactive,
        floor        : pure def,
        graph        : G.empty,
        vertModifier : def,
        mouseMove    : empty
        }

newtype GraphEditor v w = GraphEditor {
    graph      :: Event (Graph v w),
    delete     :: Event SceneTapEvent,
    isDragging :: Event Boolean
    }

derive instance newtypeGraphEditor :: Newtype (GraphEditor v w) _

_graph :: forall t a r. Newtype t { graph :: a | r } => Lens' t a
_graph = _Newtype <<< prop (SProxy :: SProxy "graph")


-- create vertex markers for an array of vertices
mkVertMarkerPoints :: forall v w. HasUUID v => Modifier v -> Dynamic ActiveMode -> Dynamic (Maybe UUID) -> Graph v w -> UUIDMap (VertMarkerPoint UUID v)
mkVertMarkerPoints m act actMarker graph = mapWithIndex (\i v -> mkVertMarkerPoint m act actMarker (Tuple v i)) ivsMap
    where ivsMap = fromFoldable $ f <$> vertices graph
          f v = Tuple (v ^. idLens) v

setupVertMarkers :: forall e v. Vector v => HasUUID v => Dynamic (UUIDMap (VertMarkerPoint UUID v)) -> Node e (Dynamic (UUIDMap (VertMarker UUID v)))
setupVertMarkers = renderDynamic


graphMidPoints :: forall v w. Ord v => HasUUID v => Vector v => Dynamic ActiveMode -> Graph v w -> Effect (List (MidMarkerPoint UUID v))
graphMidPoints actDyn = traverse midP <<< edges
    where midP l = do
              i <- genUUID
              pure $ MidMarkerPoint {
                  position : lineCenter l,
                  index    : i,
                  vert1    : l ^. _start,
                  vert2    : l ^. _end,
                  active   : actDyn
                  }

-- | render all middle markers
setupMidMarkers :: forall e v w. Ord v => HasUUID v => Vector v => Dynamic ActiveMode -> Event (Graph v w) -> Node e (Event (MidMarkerPoint UUID v))
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
    
    pure $ polygonAround 1.0 <<< view _position <$> addedPntEvt


-- update a vertex in a graph
dragGraphVert :: forall v w. HasUUID v => Ord v => Default w => v -> Graph v w -> Graph v w
dragGraphVert v g = foldl addEdge (insertVertex v $ deleteVertex v g) (adjacent v g)
    where addEdge g' v' = insertEdge v v' def g'

-- add a new vertex and edges based on clicked midmarker
addVert :: forall v w. Ord v => Default w => MidMarkerPoint UUID v -> Graph v w -> Graph v w
addVert mp g = let n = mp ^. _position
                   n1 = mp ^. _vert1
                   n2 = mp ^. _vert2
               in insertEdge n n1 def $ insertEdge n n2 def $ insertVertex n $ deleteEdge n1 n2 g


createGraphEditor :: forall e v w. Default v => Ord v => HasUUID v => Vector v => Default w => GraphEditorConf v w -> Node e (GraphEditor v w)
createGraphEditor cfg = do
    let graph  = cfg ^. _graph
        active = cfg ^. _active

    defAct <- liftEffect $ current active
    
    fixNodeDWith defAct \graphActive ->
        fixNodeDWith graph \graphDyn ->
            fixNodeDWith Nothing \actMarkerDyn -> do                
                -- setup vertices markers
                let vertMarkerPntsDyn = mkVertMarkerPoints (cfg ^. _vertModifier) graphActive actMarkerDyn <$> graphDyn
                vertMarkersDyn <- setupVertMarkers vertMarkerPntsDyn

                -- events for active vertex marker
                let newActMarkerEvt = getVertMarkerActiveStatus vertMarkersDyn
                    dragEvt = latestEvt $ anyEvt <<< map (view _position) <$> vertMarkersDyn

                    -- apply the dragged new vertex to the graph
                    graphAfterDragEvt = sampleDyn graphDyn $ dragGraphVert <$> dragEvt

                    -- new graph after dragging and adding/deleting
                    newGraphEvt = multicast $ dynEvent graphDyn <|> graphAfterDragEvt

                    midActive = lift2 (\pa am -> pa && fromBoolean (am == Nothing)) graphActive actMarkerDyn

                    floorPolyDyn = floorPlanHousePoints <$> cfg ^. _floor
                -- create mid markers for adding new vertices
                toAddEvt <- setupMidMarkers midActive newGraphEvt
                -- setup the polygon adder
                newPolyEvt <- setupPolyAdder floorPolyDyn (graphPoints <$> graphDyn) cfg

                let graphAfterAdd = sampleOn newGraphEvt $ addVert <$> toAddEvt
                    graphAfterAddPoly = sampleOn newGraphEvt $ addPolygon <$> newPolyEvt

                    -- get delete event of tapping on a marker
                    delEvts = map snd $ latestEvt $ getTapEvt <$> vertMarkersDyn
                    graphAfterDel = sampleOn newGraphEvt $ deleteVertex <$> delEvts

                    -- update the real graph after adding/deleting vertex
                    graphEvt = multicast $ graphAfterAdd <|> graphAfterDel <|> graphAfterAddPoly

                    graphActEvt = dynEvent active <|> (const Active <$> graphEvt)

                -- create the graph delete button
                graphDel <- mkPolyDelMarker (graphCenter <$> newGraphEvt) graphActive
                
                let editor = GraphEditor {
                        graph      : skip 1 newGraphEvt,
                        delete     : multicast $ graphDel ^. _tapped,
                        isDragging : multicast $ getVertMarkerDragging vertMarkersDyn
                        }
                pure { input: newActMarkerEvt, output : { input: graphEvt, output : { input: graphActEvt, output : editor }}}
    
