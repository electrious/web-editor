module Algorithm.Earcut (triangulatePoly) where

import Prelude

import Data.Array (concatMap)
import Data.Lens ((^.))
import Model.Polygon (Polygon, _polyVerts)
import Model.Roof.RoofPlate (vecArr)
import Three.Math.Vector (class Vector, getVector)

-- wrapper for the Earcut polygon triangulation algorithm in JS
foreign import earcut :: Array Number -> Array Int

triangulatePoly :: forall v. Vector v => Polygon v -> Array Int
triangulatePoly p = earcut $ concatMap f $ p ^. _polyVerts
    where f = vecArr <<< getVector
