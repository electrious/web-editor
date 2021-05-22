module Smarthouse.Algorithm.Roofs where

import Prelude

import Data.Lens (Lens', view, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), elem, singleton, sortBy, (:))
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as S
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Editor.Common.Lenses (_height, _position)
import Effect (Effect)
import Math.Angle (Angle, degreeVal, tan)
import Math.LineSeg (LineSeg, _end, _start, direction)
import Model.Polygon (newPolygon)
import Model.SmartHouse.Roof (Roof, _subtrees, createRoofFrom)
import SmartHouse.Algorithm.Edge (Edge(..), _leftVertex, _line, _rightVertex)
import SmartHouse.Algorithm.LAV (_edges)
import Smarthouse.Algorithm.Subtree (Subtree, _source)
import Three.Math.Vector (Vector3, mkVec3, (<**>), (<+>), (<->), (<.>))


scaleFactor :: Angle -> Number
scaleFactor a | degreeVal a < 90.0 = tan a
              | otherwise          = 0.0

upVec :: Vector3
upVec = mkVec3 0.0 0.0 1.0

distanceAlong :: Vector3 -> LineSeg Vector3 -> Number
distanceAlong p e = (p <-> e ^. _start) <.> direction e

-- data accumulated to generate a single roof
newtype RoofData = RoofData {
    subtrees  :: Set Subtree,
    edgeNodes :: List Vector3,
    edge      :: Edge
    }

derive instance newtypeRoofData :: Newtype RoofData _

_edgeNodes :: forall t a r. Newtype t { edgeNodes :: a | r } => Lens' t a
_edgeNodes = _Newtype <<< prop (SProxy :: SProxy "edgeNodes")

_edge :: forall t a r. Newtype t { edge :: a | r } => Lens' t a
_edge = _Newtype <<< prop (SProxy :: SProxy "edge")

-- all nodes in the roof polygon of an edge
treesForEdge :: Edge -> Set Subtree -> Set Subtree
treesForEdge e ts = S.filter f ts
    where f = elem (e ^. _line) <<< view _edges

roofDataForEdge :: Edge -> Set Subtree -> RoofData
roofDataForEdge e ts =
    let lv = e ^. _leftVertex <<< _position
        rv = e ^. _rightVertex <<< _position
    in RoofData {
        subtrees  : treesForEdge e ts,
        edgeNodes : (lv : rv : Nil),
        edge      : e
        }

-- merge roofdata of two edges with the MergedNode position
mergeRoofDataWith :: Vector3 -> RoofData -> RoofData -> RoofData
mergeRoofDataWith node lr rr =
    let lns = lr ^. _edgeNodes
        rns = rr ^. _edgeNodes
    in RoofData {
        subtrees  : S.union (lr ^. _subtrees) (rr ^. _subtrees),
        edgeNodes : lns <> singleton node <> rns,
        edge      : lr ^. _edge
        }

sortedNodes :: Edge -> Angle -> List Subtree -> List Vector3
sortedNodes e slope ts =
    let s       = scaleFactor slope
        mkP t   = t ^. _source <+> (upVec <**> (t ^. _height * s))
        edge    = e ^. _line
        g t1 t2 = comparing (flip distanceAlong edge) t2 t1
    in sortBy g $ mkP <$> ts

-- find polygon for an edge
roofForEdge :: Angle -> RoofData -> Effect Roof
roofForEdge slope rd = do
    let sorted = sortedNodes (rd ^. _edge) slope $ S.toUnfoldable $ rd ^. _subtrees
        nodes  = sorted <> rd ^. _edgeNodes
    createRoofFrom (newPolygon nodes) (rd ^. _subtrees)

generateRoofs :: Angle -> Set Subtree -> List Edge -> Effect (List Roof)
generateRoofs slope ts = traverse (roofForEdge slope ts)
