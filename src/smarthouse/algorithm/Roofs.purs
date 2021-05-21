module Smarthouse.Algorithm.Roofs where

import Prelude

import Data.Lens (view, (^.))
import Data.List (List(..), elem, singleton, sortBy)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as S
import Data.Traversable (traverse)
import Data.UUID (UUID)
import Editor.Common.Lenses (_height, _id)
import Effect (Effect)
import Math.Angle (Angle, degreeVal, tan)
import Math.LineSeg (LineSeg, _end, _start, direction)
import Model.Polygon (newPolygon)
import Model.SmartHouse.Roof (Roof, createRoofFrom)
import Model.UUID (class HasUUID, idLens)
import SmartHouse.Algorithm.Edge (Edge, _line)
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
    id        :: UUID,
    subtrees  :: Set Subtree,
    edges     :: List Edge
    }

derive instance newtypeRoofData :: Newtype RoofData _
instance hasUUIDRoofData :: HasUUID RoofData where
    idLens = _id
instance eqRoofData :: Eq RoofData where
    eq r1 r2 = r1 ^. idLens == r2 ^. idLens
instance ordRoofData :: Ord RoofData where
    compare = comparing (view idLens)

-- all nodes in the roof polygon of an edge
treesForEdge :: Edge -> Set Subtree -> Set Subtree
treesForEdge e ts = S.filter f ts
    where f = elem (e ^. _line) <<< view _edges

sortedNodes :: Edge -> Angle -> List Subtree -> List Vector3
sortedNodes e slope ts =
    let s       = scaleFactor slope
        mkP t   = t ^. _source <+> (upVec <**> (t ^. _height * s))
        edge    = e ^. _line
        g t1 t2 = comparing (flip distanceAlong edge) t2 t1
    in sortBy g $ mkP <$> ts

-- find polygon for an edge
roofForEdge :: Angle -> Set Subtree -> Edge -> Effect Roof
roofForEdge slope ts e = do
    let trees  = treesForEdge e ts
        sorted = sortedNodes e slope $ S.toUnfoldable trees
        start  = e ^. _line <<< _start
        end    = e ^. _line <<< _end
        nodes  = Cons end (sorted <> singleton start)
    createRoofFrom (newPolygon nodes) trees

generateRoofs :: Angle -> Set Subtree -> List Edge -> Effect (List Roof)
generateRoofs slope ts = traverse (roofForEdge slope ts)
