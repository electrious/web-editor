module SmartHouse.Algorithm.Skeleton where

import Prelude

import Algorithm.MeshFlatten (_vertex)
import Control.Monad.RWS (get)
import Data.Compactable (compact)
import Data.Filterable (filter)
import Data.Foldable (minimumBy)
import Data.Lens (view, (^.))
import Data.List (List(..), fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Editor.Common.Lenses (_distance, _position)
import Effect.Class (liftEffect)
import Math (abs)
import Math.Line (_direction, _origin, intersection)
import Math.LineSeg (LineSeg, _start, direction, distToLineSeg)
import Math.LineSeg as S
import Math.Utils (approxSame, epsilon)
import Model.UUID (idLens)
import SmartHouse.Algorithm.Edge (Edge, _leftBisector, _line, _rightBisector)
import SmartHouse.Algorithm.Event (EdgeE, PointEvent(..), _intersection, _vertexA, _vertexB, edgeE, intersectionPoint, splitE)
import SmartHouse.Algorithm.LAV (LAV, SLAV, _edges, _vertices, delLav, getLav, invalidateVertex, nextVertex, prevVertex, unifyVerts, updateLav)
import SmartHouse.Algorithm.Vertex (Vertex, _bisector, _cross, _isReflex, _lavId, _leftEdge, _rightEdge, ray)
import Smarthouse.Algorithm.Subtree (Subtree, subtree)
import Three.Math.Vector (Vector3, dist, length, normal, (<**>), (<+>), (<->), (<.>))


-- a potential b is at the intersection of between our own bisector and the bisector of the
-- angle between the tested edge and any one of our own edges.
-- we choose the "less parallel" edge (in order to exclude a potentially parallel edge)
intersectP :: LineSeg Vector3 -> LineSeg Vector3 -> Edge -> Maybe Vector3
intersectP lEdge rEdge e = let eVec      = direction $ e ^. _line
                               leftdot   = abs $ direction lEdge <.> eVec
                               rightdot  = abs $ direction rEdge <.> eVec
                               selfEdge  = if leftdot < rightdot then lEdge else rEdge
                           in S.intersection selfEdge (e ^. _line)

-- locate candidate b
locateB :: Vertex -> Tuple Edge Vector3 -> Maybe (Tuple Edge Vector3)
locateB v (Tuple e i) = let linVec   = normal $ v ^. _position <-> i
                            edVec    = direction $ e ^. _line
                            edVec2   = if linVec <.> edVec < 0.0 then edVec <**> (-1.0) else edVec
                            -- bisector of the tested edge e and self edge of v
                            bisecVec = edVec2 <+> linVec
                        in if length bisecVec == 0.0
                           then Nothing
                           else let bisector = ray i bisecVec
                                in Tuple e <$> intersection bisector (v ^. _bisector)

-- check eligibility of b
-- valid b should lie within the area limited by the edge and the bisectors of its two vertices
validB :: Tuple Edge Vector3 -> Boolean
validB (Tuple e b) = let xleft = _cross (normal $ e ^. _leftBisector <<< _direction)
                                        (normal $ b <-> e ^. _leftBisector <<< _origin) > (- epsilon)
                         xright = _cross (normal $ e ^. _rightBisector <<< _direction)
                                         (normal $ b <-> e ^. _rightBisector <<< _origin) < epsilon
                         xedge = _cross (direction $ e ^. _line) (b <-> e ^. _line <<< _start) < epsilon
                     in xleft && xright && xedge

-- next event for a reflex vertex
nextEvtForReflex :: Vertex -> SLAV (List PointEvent)
nextEvtForReflex v = do
    originEdges <- view _edges <$> get
    let lEdge = v ^. _leftEdge
        rEdge = v ^. _rightEdge

        -- check if an edge is the left/right edge of the vertex v
        notNearby e = not $ e ^. _line == lEdge || e ^. _line == rEdge
        -- all edges not connected to vertex v
        edges = fromFoldable $ filter notNearby originEdges
        
        notV i = not $ approxSame i $ v ^. _position

        -- find a valid intersection point
        validIntersectP :: Edge -> Maybe (Tuple Edge Vector3)
        validIntersectP e = intersectP lEdge rEdge e >>= (\i -> if notV i then Just $ Tuple e i else Nothing)

        findValidB :: Tuple Edge Vector3 -> Maybe (Tuple Edge Vector3)
        findValidB t = locateB v t >>= (\r -> if validB r then Just r else Nothing)

        mkSplitEvt :: Tuple Edge Vector3 -> PointEvent
        mkSplitEvt (Tuple e b) = splitE (distToLineSeg b (e ^. _line)) b v e

    pure $ compact $ (validIntersectP >=> findValidB >>> map mkSplitEvt) <$> edges

-- next event for a vertex
nextEvent :: LAV -> Vertex -> SLAV (Maybe PointEvent)
nextEvent lav v = do
    evts <- if v ^. _isReflex then nextEvtForReflex v else pure Nil
    let prevV = prevVertex v lav
        nextV = nextVertex v lav
        -- intersection of a vertex's bisector with v's bisector
        intersectWith = view _bisector >>> intersection (v ^. _bisector)
        iPrev = prevV >>= intersectWith
        iNext = nextV >>= intersectWith

        mkPrevEdgeEvt e pv pi = edgeE (distToLineSeg pi e) pi pv v
        pEvt = mkPrevEdgeEvt (v ^. _leftEdge) <$> prevV <*> iPrev
        mkNextEdgeEvt e nv ni = edgeE (distToLineSeg ni e) ni v nv
        nEvt = mkNextEdgeEvt (v ^. _rightEdge) <$> nextV <*> iNext

        allEvts = append (fromFoldable (compact [pEvt, nEvt])) evts
        distF e = dist (v ^. _position) (intersectionPoint e)
    pure $ minimumBy (comparing distF) allEvts



-- check if an edge event's two vertices A, B and A's predecessor C forms a triangle ABC
isTriangle :: EdgeE -> LAV -> Boolean
isTriangle e lav = prevVertex (e ^. _vertexA) lav == nextVertex (e ^. _vertexB) lav

lavForEvt :: PointEvent -> SLAV (Maybe LAV)
lavForEvt (EdgeEvent e) = getLav $ e ^. _vertexA <<< _lavId
lavForEvt (SplitEvent e) = getLav $ e ^. _vertex <<< _lavId

-- handle edge event
handleEdgeEvent :: LAV -> EdgeE -> SLAV (Tuple Subtree (List PointEvent))
handleEdgeEvent lav e =
    if isTriangle e lav
        then do let vs = lav ^. _vertices
                    sinks = fromFoldable $ view _position <$> vs

                -- delete this LAV and invalidate all vertices in it
                delLav (lav ^. idLens)
                void $ traverse invalidateVertex vs

                pure $ Tuple (subtree (e ^. _intersection) (e ^. _distance) sinks) Nil
        else do let va = e ^. _vertexA
                    vb = e ^. _vertexB
                Tuple newLav newV <- liftEffect $ unifyVerts va vb (e ^. _intersection) lav
                invalidateVertex va
                invalidateVertex vb

                let sinks = fromFoldable [va ^. _position, vb ^. _position]
                newEvt <- nextEvent newLav newV
                let evts = fromFoldable $ compact [newEvt]

                updateLav newLav
                
                pure $ Tuple (subtree (e ^. _intersection) (e ^. _distance) sinks) evts
