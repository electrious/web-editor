module Algorithm.Earcut (triangulatePoly) where

import Prelude

import Data.Array (concatMap)
import Data.Lens ((^.))
import Model.Polygon (Polygon, _polyVerts)
import Model.Roof.RoofPlate (vecArr)
import Three.Core.TypedArray (Uint16Array)
import Three.Math.Vector (class Vector, getVector)

-- wrapper for the Earcut polygon triangulation algorithm in JS
foreign import earcut :: Array Number -> Uint16Array

triangulatePoly :: forall v. Vector v => Polygon v -> Uint16Array
triangulatePoly p = earcut $ concatMap (vecArr <<< getVector) (p ^. _polyVerts)
