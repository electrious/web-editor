module Smarthouse.Algorithm.RoofGeneration where

import Prelude

import Data.Foldable (foldl)
import Data.Lens (Lens', view, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), elem, singleton, sortBy, (:))
import Data.Map as M
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as S
import Data.Traversable (traverse)
import Data.UUID (UUID)
import Data.UUIDMap (UUIDMap)
import Data.UUIDMap as UM
import Editor.Common.Lenses (_edge, _edges, _id, _normal, _position, _slope)
import Effect (Effect)
import Math.LineSeg (LineSeg, _start, direction)
import Model.Polygon (newPolygon)
import Model.SmartHouse.Roof (Roof, _subtrees, createRoofFrom)
import Model.UUID (class HasUUID, idLens)
import SmartHouse.Algorithm.Edge (Edge, _leftVertex, _lineEdge, _rightVertex)
import SmartHouse.Algorithm.EdgeInfo (_line)
import SmartHouse.Algorithm.VertNode (VertNode)
import Smarthouse.Algorithm.Subtree (Subtree, SubtreeType(..), _source, _subtreeType, mergedEdge, normalSubtree)
import Three.Math.Vector (Vector3, mkVec3, (<->), (<.>))
import Type.Proxy (Proxy(..))

upVec :: Vector3
upVec = mkVec3 0.0 0.0 1.0

distanceAlong :: Vector3 -> LineSeg Vector3 -> Number
distanceAlong p e = (p <-> e ^. _start) <.> direction e

-- data accumulated to generate a single roof
newtype RoofData = RoofData {
    id        :: UUID,
    subtrees  :: Set Subtree,
    edgeNodes :: List Vector3,
    edge      :: Edge   -- used only for calculating distance to edge
    }

derive instance newtypeRoofData :: Newtype RoofData _
instance hasUUIDRoofData :: HasUUID RoofData where
    idLens = _id
instance eqRoofData :: Eq RoofData where
    eq r1 r2 = r1 ^. idLens == r2 ^. idLens
instance ordRoofData :: Ord RoofData where
    compare = comparing (view idLens)

_edgeNodes :: forall t a r. Newtype t { edgeNodes :: a | r } => Lens' t a
_edgeNodes = _Newtype <<< prop (Proxy :: Proxy "edgeNodes")

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
        edgeNodes : (lv : rv : Nil),
        edge      : e
        }

-- merge roofdata of two edges with the MergedNode position
mergeRoofDataWith :: Subtree -> RoofData -> RoofData -> RoofData
mergeRoofDataWith t lr rr =
    let lns = lr ^. _edgeNodes
        rns = rr ^. _edgeNodes
    in RoofData {
        id        : lr ^. idLens,
        subtrees  : S.union (lr ^. _subtrees) (rr ^. _subtrees),
        edgeNodes : lns <> singleton (t ^. _source <<< _position) <> rns,
        edge      : lr ^. _edge
        }

sortedNodes :: Edge -> List Subtree -> List VertNode
sortedNodes e ts =
    let edge    = e ^. _lineEdge
        g t1 t2 = comparing (flip distanceAlong edge <<< view _position) t2 t1
    in sortBy g $ view _source <$> ts

-- find polygon for an edge
roofForEdge :: RoofData -> Effect Roof
roofForEdge rd = createRoofFrom (newPolygon nodes) (rd ^. _subtrees) (rd ^. _edge) (rd ^. _edge <<< _normal) slope
    where slope  = rd ^. _edge <<< _line <<< _slope
          sorted = sortedNodes (rd ^. _edge) $ S.toUnfoldable $ rd ^. _subtrees
          nodes  = (view _position <$> sorted) <> rd ^. _edgeNodes

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
generateRoofs :: Set Subtree -> List Edge -> Effect (List Roof)
generateRoofs ts edges = traverse roofForEdge newRds
    where -- MergedNode subtrees
          mts = S.filter (not <<< normalSubtree) ts

          -- map from edge id to roof data
          rdm = UM.fromFoldable $ roofDataForEdge ts <$> edges

          newRDM = foldl procMerge rdm mts

          newRds = S.toUnfoldable $ S.fromFoldable $ M.values newRDM
