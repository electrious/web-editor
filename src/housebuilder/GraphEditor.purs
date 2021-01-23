module Editor.HouseBuilder.GraphEditor where

import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Custom.Mesh (TappableMesh)
import Data.Array (foldl)
import Data.Default (class Default, def)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Graph (Graph, adjacent, deleteEdge, deleteVertex, insertEdge, insertVertex, size, vertices)
import Data.Graph as G
import Data.Int (toNumber)
import Data.Lens (Lens', view, (^.), (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List, concatMap)
import Data.Map (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd)
import Data.UUID (UUID, genUUID)
import Data.UUIDMap (UUIDMap)
import Editor.Common.Lenses (_active, _name, _position, _tapped)
import Editor.MarkerPoint (MidMarker, MidMarkerPoint(..), Modifier, VertMarker, VertMarkerPoint, _vert1, _vert2, getVertMarkerActiveStatus, getVertMarkerDragging, mkVertMarkerPoint)
import Editor.PolygonEditor (_vertModifier, getTapEvt)
import Editor.SceneEvent (SceneTapEvent)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, current, dynEvent, latestEvt, sampleDyn, step)
import FRP.Event (Event, keepLatest, sampleOn)
import FRP.Event.Extra (anyEvt, multicast, performEvent, skip)
import Math.Line (Line, _end, _start, lineCenter, mkLine)
import Model.ActiveMode (ActiveMode(..), fromBoolean, isActive)
import Model.UUID (class HasUUID, idLens)
import Rendering.DynamicNode (renderDynamic, renderEvent)
import Rendering.Node (Node, _visible, fixNodeDWith, tapMesh)
import Three.Core.Geometry (CircleGeometry, mkCircleGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial)
import Three.Math.Vector (class Vector, getVector, (<**>), (<+>))

-- | get all edges in the graph
edges :: forall a w. Ord a => Graph a w -> List (Line a)
edges g = concatMap f $ vertices g
    where f v = mkLine v <$> adjacent v g


graphCenter :: forall v w. Default v => Vector v =>  Graph v w -> v
graphCenter g = (foldl (<+>) def (vertices g)) <**> (1.0 / l)
    where l = toNumber $ size g


newtype GraphEditorConf v w = GraphEditorConf {
    active       :: Dynamic ActiveMode,
    graph        :: Graph v w,
    vertModifier :: Modifier v
    }

derive instance newtypeGraphEditorConf :: Newtype (GraphEditorConf v w) _
instance defaultGraphEditorConf :: Ord v => Default (GraphEditorConf v w) where
    def = GraphEditorConf {
        active       : pure Inactive,
        graph        : G.empty,
        vertModifier : def
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

                -- create mid markers for adding new vertices
                toAddEvt <- setupMidMarkers midActive newGraphEvt
                let graphAfterAdd = sampleOn newGraphEvt $ addVert <$> toAddEvt

                    -- get delete event of tapping on a marker
                    delEvts = map snd $ latestEvt $ getTapEvt <$> vertMarkersDyn
                    graphAfterDel = sampleOn newGraphEvt $ deleteVertex <$> delEvts

                    -- update the real graph after adding/deleting vertex
                    graphEvt = multicast $ graphAfterAdd <|> graphAfterDel

                    graphActEvt = dynEvent active <|> (const Active <$> graphEvt)

                -- create the graph delete button
                graphDel <- mkPolyDelMarker (graphCenter <$> newGraphEvt) graphActive
                
                let editor = GraphEditor {
                        graph      : skip 1 newGraphEvt,
                        delete     : multicast $ graphDel ^. _tapped,
                        isDragging : multicast $ getVertMarkerDragging vertMarkersDyn
                        }
                pure { input: newActMarkerEvt, output : { input: graphEvt, output : { input: graphActEvt, output : editor }}}
    
