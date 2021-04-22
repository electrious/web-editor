module Algorithm.Earcut (triangulatePoly) where

import Prelude

import Data.Array (concatMap, length)
import Data.Lens ((^.))
import Effect.Unsafe (unsafePerformEffect)
import Model.Polygon (Polygon, _polyVerts)
import Model.Roof.RoofPlate (vecArr)
import Three.Core.Face3 (Face3, mkFace3)
import Three.Math.Vector (class Vector, getVector)

-- wrapper for the Earcut polygon triangulation algorithm in JS
foreign import earcut :: Array Number -> Array Face3

triangulatePoly :: forall v. Vector v => Polygon v -> Array Face3
triangulatePoly p = if length vs > 3
                    then earcut $ concatMap (vecArr <<< getVector) vs
                    else [unsafePerformEffect $ mkFace3 0 1 2]
    where vs = p ^. _polyVerts
