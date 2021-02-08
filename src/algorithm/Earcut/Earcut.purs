module Algorithm.Earcut (triangulatePoly) where

import Prelude

import Data.Array (concatMap)
import Data.Lens ((^.))
import Model.Polygon (Polygon, _polyVerts)
import Model.Roof.RoofPlate (vecArr)
import Three.Core.Face3 (Face3)
import Three.Math.Vector (class Vector, getVector)

-- wrapper for the Earcut polygon triangulation algorithm in JS
foreign import earcut :: Array Number -> Array Face3

triangulatePoly :: forall v. Vector v => Polygon v -> Array Face3
triangulatePoly p = earcut $ concatMap f $ p ^. _polyVerts
    where f = vecArr <<< getVector
