module Editor.HouseBuilder.GraphEditor where

import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Plus (empty)
import Data.Array (foldl)
import Data.Default (class Default, def)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Graph (Graph, adjacent, deleteVertex, insertEdge, insertVertex, vertices)
import Data.Graph as G
import Data.Lens (Lens', view, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List, concatMap)
import Data.Map (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.UUID (UUID)
import Data.UUIDMap (UUIDMap)
import Editor.Common.Lenses (_active, _position)
import Editor.MarkerPoint (MidMarker, MidMarkerPoint, Modifier, VertMarker, VertMarkerPoint, getVertMarkerActiveStatus, midMarkerPoints, mkVertMarkerPoint)
import Editor.PolygonEditor (_vertModifier, getTapEvt)
import Editor.SceneEvent (SceneTapEvent)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, current, dynEvent, latestEvt, sampleDyn)
import FRP.Event (Event, keepLatest)
import FRP.Event.Extra (anyEvt, multicast)
import Math.Line (Line, lineCenter, mkLine)
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

setupVertMarkers :: forall e v w. Vector v => HasUUID v => Modifier v -> Dynamic ActiveMode -> Dynamic (Maybe UUID) -> Dynamic (Graph v w) -> Node e (Dynamic (UUIDMap (VertMarker UUID v)))
setupVertMarkers m act actMarker graphDyn = renderDynamic $ mkVertMarkerPoints m act actMarker <$> graphDyn


graphMidPoints :: forall v w. Ord v => HasUUID v => Vector v => Graph v w -> List (Tuple UUID v)
graphMidPoints = map midP <<< edges
    where midP l = let c = lineCenter l
                   in Tuple (c ^. idLens) c

-- | render all middle markers
setupMidMarkers :: forall e v w. Ord v => HasUUID v => Vector v => Dynamic ActiveMode -> Event (Graph v w) -> Node e (Event (MidMarkerPoint UUID v))
setupMidMarkers actDyn graphEvt = do
    let mPointsEvt = midMarkerPoints actDyn <<< graphMidPoints <$> graphEvt
    markers :: (Event (List (MidMarker UUID v))) <- renderEvent mPointsEvt
    pure $ keepLatest $ getTapEvt <$> markers


-- update a vertex in a graph
dragGraphVert :: forall v w. HasUUID v => Ord v => Default w => v -> Graph v w -> Graph v w
dragGraphVert v g = foldl addEdge (insertVertex v $ deleteVertex v g) (adjacent v g)
    where addEdge g' v' = insertEdge v v' def g'

createGraphEditor :: forall e v w. Default v => Ord v => HasUUID v => Vector v => Default w => GraphEditorConf v w -> Node e (GraphEditor v w)
createGraphEditor cfg = do
    let graph  = cfg ^. _graph
        active = cfg ^. _active

    defAct <- liftEffect $ current active
    
    fixNodeDWith defAct \graphActive ->
        fixNodeDWith graph \graphDyn ->
            fixNodeDWith Nothing \actMarkerDyn -> do
                -- setup vertices markers
                vertMarkersDyn <- setupVertMarkers (cfg ^. _vertModifier) graphActive actMarkerDyn graphDyn

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

                
                let editor = GraphEditor {
                        graph      : empty,
                        delete     : empty,
                        isDragging : empty
                        }
                pure { input: empty, output : { input: empty, output : { input: empty, output : editor }}}
    
