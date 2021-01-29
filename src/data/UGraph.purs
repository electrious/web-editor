module Data.UGraph where

import Prelude

import Data.Default (class Default, def)
import Data.Filterable (filter)
import Data.Foldable (foldl)
import Data.Graph as G
import Data.Int (toNumber)
import Data.Lens ((^.))
import Data.List (List(..))
import Data.Tuple (Tuple(..), snd)
import Math.Line (Line, _end, _start, mkLine)
import Model.Polygon (Polygon, _polyVerts, polyEdges)
import Three.Math.Vector (class Vector, (<+>), (<**>))

type UGraph = G.Graph

empty :: forall v w. Ord v => UGraph v w
empty = G.empty

insertVertex :: forall v w. Ord v => v -> UGraph v w -> UGraph v w
insertVertex = G.insertVertex

deleteVertex :: forall v w. Ord v => v -> UGraph v w -> UGraph v w
deleteVertex = G.deleteVertex

insertEdge :: forall v w. Ord v => v -> v -> w -> UGraph v w -> UGraph v w
insertEdge v1 v2 w g = G.insertEdge v1 v2 w $ G.insertEdge v2 v1 w g

deleteEdge :: forall v w. Ord v => v -> v -> UGraph v w -> UGraph v w
deleteEdge v1 v2 g = G.deleteEdge v1 v2 $ G.deleteEdge v2 v1 g

-- | get all edges in the graph and remove all duplicated edge
edges :: forall v w. Ord v => UGraph v w -> List (Line v)
edges g = snd $ foldl edgesFrom (Tuple g Nil) $ G.vertices g
    where edgesFrom arg@(Tuple g' _) v = foldl (f v) arg (G.adjacent v g')
          f v (Tuple g' es) vv = let l = mkLine v vv
                                 in Tuple (G.deleteEdge vv v g') (Cons l es)

-- | get center point of the graph
graphCenter :: forall v w. Default v => Vector v => UGraph v w -> v
graphCenter g = (foldl (<+>) def (G.vertices g)) <**> (1.0 / l)
    where l = toNumber $ G.size g

-- | get all vertice and edge mid points in a graph
graphPoints :: forall v w. Ord v => Vector v => UGraph v w -> List v
graphPoints g = G.vertices g <> (center <$> edges g)
    where center l = (l ^. _start <+> l ^. _end) <**> 0.5


-- add a new polygon to the graph
addPolygon :: forall v w. Ord v => Vector v => Default w => Polygon v -> UGraph v w -> UGraph v w
addPolygon poly = addEdges (polyEdges poly) <<< addVerts (poly ^. _polyVerts)
    where addEdges = flip (foldl (\g (Tuple p1 p2) -> insertEdge p1 p2 def g))
          addVerts = flip (foldl (flip G.insertVertex))


-- merge two vertices in a graph
mergeVertices :: forall v w. Ord v => Default w => v -> v -> v -> UGraph v w -> UGraph v w
mergeVertices v1 v2 v g = let ns1 = G.adjacent v1 g
                              ns2 = G.adjacent v2 g
                              ns  = filter (\v' -> v' /= v1 && v' /= v2) $ ns1 <> ns2
                              addEdges = flip (foldl (\g' nv -> insertEdge v nv def g'))
                          in addEdges ns $ insertVertex v $ deleteVertex v1 $ deleteVertex v2 g
