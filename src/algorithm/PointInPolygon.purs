module Algorithm.PointInPolygon (pointInPolygon, underPolygons) where


import Prelude

import Data.Foldable (class Foldable, any)
import Model.Polygon (class IsPolygon, Polygon(..), toPolygon)
import Three.Math.Vector (Vector2, Vector3, mkVec2, vecX, vecY)

-- | test if a 2d point is inside a polygon
foreign import pointInPoly :: Array Vector2 -> Vector2 -> Boolean

pointInPolygon :: Polygon -> Vector2 -> Boolean
pointInPolygon (Polygon vs) v = pointInPoly vs v

-- | check if a point is under a list of IsPolygon values
underPolygons :: forall f p. Functor f => Foldable f => IsPolygon p => f p -> Vector3 -> Boolean
underPolygons ps p = any (flip pointInPolygon flatP) polys
    where polys = toPolygon <$> ps

          -- 2D projection of the intersection point
          flatP = mkVec2 (vecX p) (vecY p)
