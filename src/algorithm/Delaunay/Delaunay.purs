module Algorithm.Delaunay.Delaunay (triangulate) where

import Prelude

import Algorithm.Delaunay.Triangle (Edge, Triangle, _rsqr, _vertex1, _vertex2, epsilon, mkTriangle, triangleEdges)
import Algorithm.Delaunay.Vertex (class Vertex, vertX, vertY)
import Data.Array as Arr
import Data.Lens ((^.))
import Data.List (List(..), foldl, reverse, sortBy, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Triple (Triple(..))
import Data.Tuple (Tuple(..))
import Editor.Common.Lenses (_x, _y)

-- | triangulate will create new triangles based on a list of old triangles
-- and new vertices.
triangulate :: forall v. Vertex v => Ord v => Array (Triangle v) -> Array v -> Array (Triangle v)
triangulate old verts = allTris $ foldl addVertex (Tuple Nil (List.fromFoldable old)) vs
    where vs = sortBy (comparing vertX) $ Set.toUnfoldable $ Set.fromFoldable verts
          allTris (Tuple completed open) = Arr.fromFoldable $ completed <> open

-- add one vertex to the computed/open triangles list
-- For each open triangle, check to see if the current point is
-- inside its circumcircle. If it is, remove the triangle and add
-- it's edges to an edge list.
addVertex :: forall v. Vertex v => Eq v => Tuple (List (Triangle v)) (List (Triangle v)) -> v -> Tuple (List (Triangle v)) (List (Triangle v))
addVertex (Tuple completed open) v = Tuple newComp (newOpen <> newTris)
    where Triple newComp newOpen edges = foldl (checkTriangle v) (Triple completed Nil Nil) open
          newTris = mkNewTriangle v <$> dedup edges

-- delete duplicated edges in a list
dedup :: forall a. Eq a => List a -> List a
dedup = g <<< foldl f (Tuple Nil Nothing)
    where f (Tuple nl Nothing) a = Tuple nl (Just a)
          f (Tuple nl (Just ov)) a = if ov == a
                                     then Tuple nl (Just ov)
                                     else Tuple (ov : nl) (Just a)
          g (Tuple nl Nothing) = reverse nl
          g (Tuple nl (Just ov)) = reverse (ov : nl)

-- create new triangle based on a vertex and an edge.
mkNewTriangle :: forall v. Vertex v => v -> Edge v -> Triangle v
mkNewTriangle v e = mkTriangle (e ^. _vertex1) (e ^. _vertex2) v

vertOnRight :: forall v. Vertex v => v -> Triangle v -> Boolean
vertOnRight v tri = dx > 0.0 && dx * dx > tri ^. _rsqr
    where dx = vertX v - tri ^. _x

vertOutside :: forall v. Vertex v => v -> Triangle v -> Boolean
vertOutside v tri = dx * dx + dy * dy - tri ^. _rsqr > epsilon
    where dx = vertX v - tri ^. _x
          dy = vertY v - tri ^. _y

type TempData v = Triple (List (Triangle v)) (List (Triangle v)) (List (Edge v))

-- If this point is to the right of this triangle's circumcircle,
-- then this triangle should never get checked again.
-- If it's outside the circumcircle, skip this triangle.
checkTriangle :: forall v. Vertex v => v -> TempData v -> Triangle v -> TempData v
checkTriangle vt (Triple cs os edges) tri | vertOnRight vt tri = Triple (tri : cs) os edges
                                          | vertOutside vt tri = Triple cs (tri : os) edges
                                          | otherwise          = Triple cs os (edges <> triangleEdges tri)
