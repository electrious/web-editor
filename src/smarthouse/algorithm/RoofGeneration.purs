module Smarthouse.Algorithm.RoofGeneration where

import Prelude

import Data.Foldable (foldl)
import Data.Graph (Graph, fromAdjacencyList, shortestPath)
import Data.Lens (Lens', view, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), concatMap, elem, singleton, (:))
import Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as S
import Data.Tuple (Tuple(..))
import Data.UUID (UUID)
import Data.UUIDMap (UUIDMap)
import Data.UUIDMap as UM
import Editor.Common.Lenses (_edge, _edges, _id, _normal, _position, _slope)
import Model.Polygon (newPolygon)
import Model.SmartHouse.Roof (Roof, createRoofFrom)
import Model.UUID (class HasUUID, idLens)
import SmartHouse.Algorithm.Edge (Edge, _leftVertex, _rightVertex)
import SmartHouse.Algorithm.EdgeInfo (_line)
import SmartHouse.Algorithm.VertNode (VertNode)
import Smarthouse.Algorithm.Subtree (Subtree, SubtreeType(..), _sinks, _source, _subtreeType, mergedEdge, normalSubtree)
import Three.Math.Vector (Vector3, mkVec3)
import Type.Proxy (Proxy(..))

upVec :: Vector3
upVec = mkVec3 0.0 0.0 1.0

-- data accumulated to generate a single roof
newtype RoofData = RoofData {
    id        :: UUID,
    subtrees  :: Set Subtree,
    leftVert  :: VertNode,
    rightVert :: VertNode,
    edgeNodes :: List VertNode,   -- all vertex nodes between left and right vertex for the single roof.
    edge      :: Edge,            -- used only for calculating distance to edge
    edges     :: List Edge        -- all edges in one roof
    }

derive instance newtypeRoofData :: Newtype RoofData _
instance hasUUIDRoofData :: HasUUID RoofData where
    idLens = _id
instance eqRoofData :: Eq RoofData where
    eq r1 r2 = r1 ^. idLens == r2 ^. idLens
instance ordRoofData :: Ord RoofData where
    compare = comparing (view idLens)

_subtrees :: forall t a r. Newtype t { subtrees :: a | r } => Lens' t a
_subtrees = _Newtype <<< prop (Proxy :: Proxy "subtrees")

_edgeNodes :: forall t a r. Newtype t { edgeNodes :: a | r } => Lens' t a
_edgeNodes = _Newtype <<< prop (Proxy :: Proxy "edgeNodes")

_leftVert :: forall t a r. Newtype t { leftVert :: a | r } => Lens' t a
_leftVert = _Newtype <<< prop (Proxy :: Proxy "leftVert")

_rightVert :: forall t a r. Newtype t { rightVert :: a | r } => Lens' t a
_rightVert = _Newtype <<< prop (Proxy :: Proxy "rightVert")

-- all nodes in the roof polygon of an edge, but omit the MergedNode
-- if the edge should be merged
treesForEdge :: Edge -> Set Subtree -> Set Subtree
treesForEdge e ts = S.filter f ts
    where f t = elem e (t ^. _edges) && not (mergedEdge e t)

roofDataForEdge :: Set Subtree -> Edge -> RoofData
roofDataForEdge ts e =
    let lv = e ^. _leftVertex
        rv = e ^. _rightVertex
    in RoofData {
        id        : e ^. idLens,
        subtrees  : treesForEdge e ts,
        leftVert  : lv,
        rightVert : rv,
        edgeNodes : Nil,
        edge      : e,
        edges     : singleton e
        }

-- merge roofdata of two edges with the MergedNode position
mergeRoofDataWith :: Subtree -> RoofData -> RoofData -> RoofData
mergeRoofDataWith t lr rr =
    let lns = lr ^. _edgeNodes
        rns = rr ^. _edgeNodes
    in RoofData {
        id        : lr ^. idLens,
        subtrees  : S.union (lr ^. _subtrees) (rr ^. _subtrees),
        leftVert  : lr ^. _leftVert,
        rightVert : rr ^. _rightVert,
        edgeNodes : lns <> (lr ^. _rightVert : t ^. _source : rr ^. _leftVert : Nil) <> rns,
        edge      : lr ^. _edge,
        edges     : lr ^. _edges <> rr ^. _edges
        }

-- convert all subtrees of a single edge into an undirected graph
mkGraph :: List Subtree -> Graph VertNode Number
mkGraph ts = fromAdjacencyList $ concatMap f ts
    where f t = let src         = t ^. _source
                    sinks       = t ^. _sinks
                    srcLst      = singleton $ mkVal src
                    posEdge     = Tuple src $ mkVal <$> sinks
                    mkNegEdge s = Tuple s srcLst
                    negEdges    = mkNegEdge <$> sinks
                in posEdge : negEdges
          mkVal n = Tuple n 1.0

-- find polygon for an edge
roofForEdge :: RoofData -> Roof
roofForEdge rd = createRoofFrom (rd ^. idLens) (newPolygon nodes) (rd ^. _edges) (rd ^. _edge <<< _normal) slope
    where slope  = rd ^. _edge <<< _line <<< _slope
          sorted = fromMaybe Nil $ shortestPath (rd ^. _rightVert) (rd ^. _leftVert) $ mkGraph $ S.toUnfoldable $ rd ^. _subtrees
          nodes  = view _position <$> (sorted <> rd ^. _edgeNodes)

procMerge :: UUIDMap RoofData -> Subtree -> UUIDMap RoofData
procMerge m t = case t ^. _subtreeType of
    NormalNode -> m
    MergedNode le re ->
        let li = le ^. idLens
            ri = re ^. idLens
            lrd = M.lookup li m
            rrd = M.lookup ri m

            newrd = mergeRoofDataWith t <$> lrd <*> rrd

        in M.update (const newrd) li $ M.update (const newrd) ri m
        
-- generate a list of Roofs and update Subtree node to 3D
generateRoofs :: Set Subtree -> List Edge -> List Roof
generateRoofs ts edges = roofForEdge <$> newRds
    where -- MergedNode subtrees
          mts = S.filter (not <<< normalSubtree) ts

          -- map from edge id to roof data
          rdm = UM.fromFoldable $ roofDataForEdge ts <$> edges

          newRDM = foldl procMerge rdm mts

          newRds = S.toUnfoldable $ S.fromFoldable $ M.values newRDM
