module Data.Graph.Extra where

import Prelude

import Data.Default (class Default, def)
import Data.Foldable (foldl)
import Data.Graph (Graph, adjacent, insertEdge, insertVertex, size, vertices)
import Data.Int (toNumber)
import Data.Lens ((^.))
import Data.List (List, concatMap)
import Data.Tuple (Tuple(..))
import Math.Line (Line, _end, _start, mkLine)
import Model.Polygon (Polygon, _polyVerts, polyEdges)
import Three.Math.Vector (class Vector, (<+>), (<**>))


-- | get all edges in the graph
edges :: forall a w. Ord a => Graph a w -> List (Line a)
edges g = concatMap f $ vertices g
    where f v = mkLine v <$> adjacent v g

-- | get center point of the graph
graphCenter :: forall v w. Default v => Vector v =>  Graph v w -> v
graphCenter g = (foldl (<+>) def (vertices g)) <**> (1.0 / l)
    where l = toNumber $ size g

-- | get all vertice and edge mid points in a graph
graphPoints :: forall v w. Ord v => Vector v => Graph v w -> List v
graphPoints g = vertices g <> (center <$> edges g)
    where center l = (l ^. _start <+> l ^. _end) <**> 0.5


-- add a new polygon to the graph
addPolygon :: forall v w. Ord v => Vector v => Default w => Polygon v -> Graph v w -> Graph v w
addPolygon poly = addEdges (polyEdges poly) <<< addVerts (poly ^. _polyVerts)
    where addEdges = flip (foldl (\g (Tuple p1 p2) -> insertEdge p1 p2 def g))
          addVerts = flip (foldl (flip insertVertex))
