module Data.UGraph where

import Prelude

import Data.Compactable (compact)
import Data.Default (class Default, def)
import Data.Filterable (filter)
import Data.Foldable (class Foldable, foldl)
import Data.Graph (adjacent, vertices)
import Data.Graph as G
import Data.Int (toNumber)
import Data.Lens (Lens', view, (%~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), (:), difference, head)
import Data.List as L
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as S
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import Math.Angle (degreeVal)
import Math.Line (Line, _end, _start, mkLine, mostParaLine, projPointWithLine)
import Model.Polygon (Polygon, _polyVerts, polyEdges)
import Three.Math.Vector (class Vector, getVector, updateVector, (<**>), (<+>))

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

-- update a vertex in a graph
updateVert :: forall v w. Ord v => Default w => v -> UGraph v w -> UGraph v w
updateVert v g = foldl addEdge (insertVertex v $ deleteVertex v g) (adjacent v g)
    where addEdge g' v' = insertEdge v v' def g'

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


-- find out if there's a nearby point so that the dragged vertex
-- can snap to to make an edge paralell to another edge in the graph
snapToParallel :: forall v w. Eq v => Ord v => Vector v => Default w => v -> UGraph v w -> UGraph v w
snapToParallel v g =
    let ls = mkLine v <$> adjacent v g  -- new edges lines start from v
        es = difference (edges g) ls    -- all other edges
        -- find a new V after snapping
        nps = head $ compact $ (flip (snapVertToPara v) es <$> ls)
        f v' = updateVert (updateVector v $ getVector v') g
    in fromMaybe g $ f <$> nps


snapVertToPara :: forall f v. Foldable f => Vector v => v -> Line v -> f (Line v) -> Maybe v
snapVertToPara v line lines = 
    let f (Tuple _ a) = degreeVal a < 10.0
        pl = fst <$> filter f (mostParaLine line lines)
    in projPointWithLine (line ^. _end) (line ^. _start) <$> pl


newtype CircleState v = CircleState {
    visited :: Set v,
    circles :: List (List v)
    }

derive instance newtypeCircleState :: Newtype (CircleState v) _
instance defaultCirlceState :: Default (CircleState v) where
    def = CircleState {
        visited : S.empty,
        circles : Nil
        }

_visited :: forall t a r. Newtype t { visited :: a | r } => Lens' t a
_visited = _Newtype <<< prop (SProxy :: SProxy "visited")

_circles :: forall t a r. Newtype t { circles :: a | r } => Lens' t a
_circles = _Newtype <<< prop (SProxy :: SProxy "circles")

-- detect all circles in a graph
allCircles :: forall v w. Ord v => UGraph v w -> List (List v)
allCircles g = view _circles $ foldl (circlesFrom g) def $ vertices g


circlesFrom :: forall v w. Ord v => UGraph v w -> CircleState v -> v -> CircleState v
circlesFrom g s from
    | G.elem from g =
        let go Nil s' path    = s'
            go (v:vs) s' path
                | S.member v (s' ^. _visited) = go vs s' path
                | L.elem v path = s' # _circles %~ (:) path
                                     # _visited %~ S.insert v
                | otherwise     = go (G.adjacent v g <> vs) (s' # _visited %~ S.insert v) (v:path)
        in go (L.singleton from) s Nil
    | otherwise = s
