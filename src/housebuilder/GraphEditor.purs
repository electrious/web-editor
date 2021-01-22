module Editor.HouseBuilder.GraphEditor where

import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Plus (empty)
import Data.Array (foldl)
import Data.Compactable (compact)
import Data.Default (class Default, def)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Graph (Graph, adjacent, deleteEdge, deleteVertex, insertEdge, insertVertex, vertices)
import Data.Graph as G
import Data.Lens (Lens', view, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List, concatMap)
import Data.Map (fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, genUUID)
import Data.UUIDMap (UUIDMap)
import Editor.Common.Lenses (_active, _position)
import Editor.MarkerPoint (MidMarker, MidMarkerPoint(..), Modifier, VertMarker, VertMarkerPoint, _vertIdx1, _vertIdx2, getVertMarkerActiveStatus, mkVertMarkerPoint)
import Editor.PolygonEditor (_vertModifier, getTapEvt)
import Editor.SceneEvent (SceneTapEvent)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, current, dynEvent, latestEvt, sampleDyn)
import FRP.Event (Event, keepLatest, sampleOn)
import FRP.Event.Extra (anyEvt, multicast, performEvent)
import Math.Line (Line, _end, _start, lineCenter, mkLine)
import Model.ActiveMode (ActiveMode(..), fromBoolean)
import Model.UUID (class HasUUID, idLens)
import Rendering.DynamicNode (renderDynamic, renderEvent)
import Rendering.Node (Node, fixNodeDWith)
import Three.Math.Vector (class Vector)

-- | get all edges in the graph
edges :: forall a w. Ord a => Graph a w -> List (Line a)
edges g = concatMap f $ vertices g
    where f v = mkLine v <$> adjacent v g


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

setupVertMarkers :: forall e v w. Vector v => HasUUID v => Dynamic (UUIDMap (VertMarkerPoint UUID v)) -> Node e (Dynamic (UUIDMap (VertMarker UUID v)))
setupVertMarkers = renderDynamic


graphMidPoints :: forall v w. Ord v => HasUUID v => Vector v => Dynamic ActiveMode -> Graph v w -> Effect (List (MidMarkerPoint UUID v))
graphMidPoints actDyn = traverse midP <<< edges
    where midP l = do
              i <- genUUID
              pure $ MidMarkerPoint {
                  position : lineCenter l,
                  index    : i,
                  vertIdx1 : l ^. _start <<< idLens,
                  vertIdx2 : l ^. _end <<< idLens,
                  active   : actDyn
                  }

-- | render all middle markers
setupMidMarkers :: forall e v w. Ord v => HasUUID v => Vector v => Dynamic ActiveMode -> Event (Graph v w) -> Node e (Event (MidMarkerPoint UUID v))
setupMidMarkers actDyn graphEvt = do
    let mPointsEvt = performEvent $ graphMidPoints actDyn <$> graphEvt
    markers :: (Event (List (MidMarker UUID v))) <- renderEvent mPointsEvt
    pure $ keepLatest $ getTapEvt <$> markers


-- update a vertex in a graph
dragGraphVert :: forall v w. HasUUID v => Ord v => Default w => v -> Graph v w -> Graph v w
dragGraphVert v g = foldl addEdge (insertVertex v $ deleteVertex v g) (adjacent v g)
    where addEdge g' v' = insertEdge v v' def g'

-- add a new vertex and edges based on clicked midmarker
addVert :: forall v w. Ord v => Default w => MidMarkerPoint UUID v -> Graph v w -> UUIDMap (VertMarkerPoint UUID v) -> Graph v w
addVert mp g vertMarkers = let n = mp ^. _position
                               n1 = view _position <$> lookup (mp ^. _vertIdx1) vertMarkers
                               n2 = view _position <$> lookup (mp ^. _vertIdx2) vertMarkers
                               delE g' n1' n2' = deleteEdge n1' n2' g'
                               -- graph after delete old edge
                               gAfterDelE = fromMaybe g $ delE g <$> n1 <*> n2
                               
                               f g' nn = insertEdge n nn def g'
                           in foldl f (insertVertex n gAfterDelE) $ compact [n1, n2]

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
                let graphAfterAdd = sampleDyn vertMarkerPntsDyn $ sampleOn newGraphEvt $ addVert <$> toAddEvt
                
                let editor = GraphEditor {
                        graph      : empty,
                        delete     : empty,
                        isDragging : empty
                        }
                pure { input: empty, output : { input: empty, output : { input: empty, output : editor }}}
    
