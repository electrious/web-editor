module Algorithm.Delaunay.Delaunay (triangulate) where

import Prelude

import Algorithm.Delaunay.Triangle (Triangle, _rsqr, _vertex1, _vertex2, epsilon, mkTriangle, triangleEdges)
import Algorithm.Delaunay.Vertex (class Vertex, vertX, vertY)
import Data.Lens ((^.))
import Data.List (List(..), (:), foldl, sortBy)
import Data.Set as Set
import Data.Triple (Triple(..))
import Data.Tuple (Tuple(..))
import Editor.Common.Lenses (_x, _y)

-- delete duplicated edges in a list
dedup :: forall a. Eq a => List a -> List a
dedup Nil = Nil
dedup l@(a : Nil) = l
dedup (a : b : as) | a == b    = dedup (b : as)
                   | otherwise = a : (dedup (b : as))

triangulate :: forall v. Vertex v => Ord v => List (Triangle v) -> List v -> List (Triangle v)
triangulate old verts = allTris $ foldl addVertex (Tuple Nil old) vs
    where vs = sortBy (comparing vertX) $ Set.toUnfoldable $ Set.fromFoldable verts
          allTris (Tuple completed open) = completed <> open

-- add one vertex to the computed/open triangles list
addVertex :: forall v. Vertex v => Eq v => Tuple (List (Triangle v)) (List (Triangle v)) -> v -> Tuple (List (Triangle v)) (List (Triangle v))
addVertex (Tuple completed open) v = Tuple newComp (newOpen <> newTris)
    where -- For each open triangle, check to see if the current point is
          -- inside its circumcircle. If it is, remove the triangle and add
          -- it's edges to an edge list.
          checkTriangle vt (Triple cs os edges) tri =
              -- If this point is to the right of this triangle's circumcircle,
              -- then this triangle should never get checked again. Remove it
              -- from the open list, add it to the closed list, and skip.
              let dx = vertX vt - tri ^. _x
                  dy = vertY vt - tri ^. _y
              in if dx > 0.0 && dx * dx > tri ^. _rsqr
              then Triple (tri : cs) os edges
              else if dx * dx + dy * dy - tri ^. _rsqr > epsilon  -- If we're outside the circumcircle, skip this triangle.
                      then Triple cs (tri : os) edges
                      else Triple cs os (edges <> triangleEdges tri)
        
          Triple newComp newOpen edges = foldl (checkTriangle v) (Triple completed Nil Nil) open
          newTris = mkT <$> dedup edges

          mkT e = mkTriangle (e ^. _vertex1) (e ^. _vertex2) v
