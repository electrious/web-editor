module Smarthouse.Algorithm.Roofs where

import Prelude

import Data.Filterable (filter)
import Data.Lens (view, (^.))
import Data.List (List(..), elem, singleton, sortBy)
import Editor.Common.Lenses (_height)
import Math.Angle (Angle, degreeVal, tan)
import Math.LineSeg (LineSeg, _end, _start, direction)
import Model.Polygon (Polygon, newPolygon)
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

-- all nodes in the roof polygon of an edge
nodesForEdge :: Edge -> Angle -> List Subtree -> List Vector3
nodesForEdge e slope ts =
    let s      = scaleFactor slope
        mkP t  = t ^. _source <+> (upVec <**> (t ^. _height * s))
        edge   = e ^. _line
        g t1 t2 = compare (distanceAlong t2 edge) (distanceAlong t1 edge)
    in sortBy g $ mkP <$> filter (elem edge <<< view _edges) ts

-- find polygon for an edge
polygonsForEdge :: Angle -> List Subtree -> Edge -> Polygon Vector3
polygonsForEdge slope ts e =
    let sorted = nodesForEdge e slope ts
        start  = e ^. _line <<< _start
        end    = e ^. _line <<< _end
        nodes  = Cons end (sorted <> singleton start)
    in newPolygon nodes

roofPolygons :: Angle -> List Subtree -> List Edge -> List (Polygon Vector3)
roofPolygons slope ts = map (polygonsForEdge slope ts)
