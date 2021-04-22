module Smarthouse.Algorithm.Roofs where

import Prelude

import Data.Filterable (filter)
import Data.Lens (view, (^.))
import Data.List (List(..), elem, mapWithIndex, singleton, sortBy)
import Data.Traversable (traverse)
import Editor.Common.Lenses (_height)
import Effect (Effect)
import Math.Angle (Angle, degreeVal, tan)
import Math.LineSeg (LineSeg, _end, _start, direction)
import Model.Polygon (newPolygon)
import Model.SmartHouse.Roof (Roof, createRoofFrom)
import SmartHouse.Algorithm.Edge (Edge, _line)
import SmartHouse.Algorithm.LAV (_edges)
import Smarthouse.Algorithm.Subtree (IndexedSubtree, Subtree, _source, getSubtree, mkIndexedSubtree)
import Three.Math.Vector (Vector3, mkVec3, (<**>), (<+>), (<->), (<.>))


scaleFactor :: Angle -> Number
scaleFactor a | degreeVal a < 90.0 = tan a
              | otherwise          = 0.0

upVec :: Vector3
upVec = mkVec3 0.0 0.0 1.0

distanceAlong :: Vector3 -> LineSeg Vector3 -> Number
distanceAlong p e = (p <-> e ^. _start) <.> direction e

-- all nodes in the roof polygon of an edge
treesForEdge :: Edge -> List Subtree -> List IndexedSubtree
treesForEdge e ts = filter (elem edge <<< view _edges <<< getSubtree) $ mapWithIndex mkIndexedSubtree ts
    where edge = e ^. _line


sortedNodes :: Edge -> Angle -> List IndexedSubtree -> List Vector3
sortedNodes e slope ts =
    let s       = scaleFactor slope
        mkP t   = t ^. _source <+> (upVec <**> (t ^. _height * s))
        edge    = e ^. _line
        g t1 t2 = compare (distanceAlong t2 edge) (distanceAlong t1 edge)
    in sortBy g $ mkP <<< getSubtree <$> ts

-- find polygon for an edge
roofForEdge :: Angle -> List Subtree -> Edge -> Effect Roof
roofForEdge slope ts e = do
    let trees  = treesForEdge e ts
        sorted = sortedNodes e slope trees
        start  = e ^. _line <<< _start
        end    = e ^. _line <<< _end
        nodes  = Cons end (sorted <> singleton start)
    createRoofFrom (newPolygon nodes) trees

generateRoofs :: Angle -> List Subtree -> List Edge -> Effect (List Roof)
generateRoofs slope ts = traverse (roofForEdge slope ts)
