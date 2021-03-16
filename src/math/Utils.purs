module Math.Utils where

import Prelude

import Data.Maybe (Maybe(..))
import Math (abs)
import Three.Math.Vector (class Vector, Vector3, mkVec3, vecX, vecY, vecZ)


-- tolerance used for calculating approximate equality
epsilon :: Number
epsilon = 0.0001


approxEqual :: Number -> Number -> Boolean
approxEqual a b = abs (a - b) < epsilon

approxSame :: forall v. Vector v => v -> v -> Boolean
approxSame p1 p2 = approxEqual (vecX p1) (vecX p2) &&
                   approxEqual (vecY p1) (vecY p2)


lineIntersection :: Vector3 -> Vector3 -> Vector3 -> Vector3 -> Maybe Vector3
lineIntersection s1 e1 s2 e2 =
    let x1 = vecX s1
        y1 = vecY s1
        x2 = vecX e1
        y2 = vecY e1
        x3 = vecX s2
        y3 = vecY s2
        x4 = vecX e2
        y4 = vecY e2
        d  = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    in if abs d < epsilon
       then Nothing
       else let n1 = x1 * y2 - y1 * x2
                n2 = x3 * y4 - y3 * x4
                x = (n1 * (x3 - x4) - (x1 - x2) * n2) / d
                y = (n1 * (y3 - y4) - (y1 - y2) * n2) / d
            in Just $ mkVec3 x y (vecZ s1)

