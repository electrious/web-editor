module Smarthouse.Algorithm.Roofs where

import Prelude

import Data.Array (foldl)
import Data.Lens (Lens', view, (%~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), elem, singleton, sortBy, (:))
import Data.Map as M
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as S
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.UUID (UUID)
import Data.UUIDMap (UUIDMap)
import Data.UUIDMap as UM
import Editor.Common.Lenses (_height, _id, _normal, _position)
import Effect (Effect)
import Math.Angle (Angle, degreeVal, tan)
import Math.LineSeg (LineSeg, _start, direction)
import Model.Polygon (newPolygon)
import Model.SmartHouse.Roof (Roof, _subtrees, createRoofFrom)
import Model.UUID (class HasUUID, idLens)
import SmartHouse.Algorithm.Edge (Edge, _leftVertex, _line, _rightVertex)
import SmartHouse.Algorithm.LAV (_edges)
import SmartHouse.Algorithm.VertNode (VertNode)
import Smarthouse.Algorithm.Subtree (Subtree, SubtreeType(..), _source, _subtreeType, mergedEdge, normalSubtree)
import Three.Math.Vector (Vector3, mkVec3, vecX, vecY, (<->), (<.>))


scaleFactor :: Angle -> Number
scaleFactor a | degreeVal a < 90.0 = tan a
              | otherwise          = 0.0

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
_edgeNodes = _Newtype <<< prop (SProxy :: SProxy "edgeNodes")

_edge :: forall t a r. Newtype t { edge :: a | r } => Lens' t a
_edge = _Newtype <<< prop (SProxy :: SProxy "edge")

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
mergeRoofDataWith :: Angle -> Subtree -> RoofData -> RoofData -> RoofData
mergeRoofDataWith slope t lr rr =
    let lns = lr ^. _edgeNodes
        rns = rr ^. _edgeNodes
    in RoofData {
        id        : lr ^. idLens,
        subtrees  : S.union (lr ^. _subtrees) (rr ^. _subtrees),
        edgeNodes : lns <> singleton (view _position $ projNodeTo3D slope t) <> rns,
        edge      : lr ^. _edge
        }

setZ :: Number -> Vector3 -> Vector3
setZ z v = mkVec3 (vecX v) (vecY v) z

-- project a subtree node source's Z to 3D value based on slope and distance to corresponding edge
projNodeTo3D :: Angle -> Subtree -> VertNode
projNodeTo3D slope t = f (t ^. _source)
    where s = scaleFactor slope
          f v = v # _position %~ setZ (v ^. _height * s)

sortedNodes :: Edge -> Angle -> List Subtree -> List VertNode
sortedNodes e slope ts =
    let edge    = e ^. _line
        g t1 t2 = comparing (flip distanceAlong edge <<< view _position) t2 t1
    in sortBy g $ projNodeTo3D slope <$> ts

-- find polygon for an edge
roofForEdge :: Angle -> RoofData -> Effect (Tuple Roof (List VertNode))
roofForEdge slope rd = do
    let sorted = sortedNodes (rd ^. _edge) slope $ S.toUnfoldable $ rd ^. _subtrees
        nodes  = (view _position <$> sorted) <> rd ^. _edgeNodes
    roof <- createRoofFrom (newPolygon nodes) (rd ^. _subtrees) (rd ^. _edge <<< _normal)
    pure $ Tuple roof sorted


procMerge :: Angle -> UUIDMap RoofData -> Subtree -> UUIDMap RoofData
procMerge slope m t = case t ^. _subtreeType of
    NormalNode -> m
    MergedNode le re ->
        let li = le ^. idLens
            ri = re ^. idLens
            lrd = M.lookup li m
            rrd = M.lookup ri m

            newrd = mergeRoofDataWith slope t <$> lrd <*> rrd

        in M.update (const newrd) li $ M.update (const newrd) ri m
        
-- generate a list of Roofs and update Subtree node to 3D
generateRoofs :: Angle -> Set Subtree -> List Edge -> Effect (Tuple (List Roof) (UUIDMap VertNode))
generateRoofs slope ts edges = do
    let -- MergedNode subtrees
        mts = S.filter (not <<< normalSubtree) ts

        -- map from edge id to roof data
        rdm = UM.fromFoldable $ roofDataForEdge ts <$> edges

        newRDM = foldl (procMerge slope) rdm mts

        newRds = S.toUnfoldable $ S.fromFoldable $ M.values newRDM

    res <- traverse (roofForEdge slope) newRds
    let roofs = fst <$> res
        trees = UM.fromFoldable $ join $ snd <$> res
    pure $ Tuple roofs trees
