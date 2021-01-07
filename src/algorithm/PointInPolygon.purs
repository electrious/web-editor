module Algorithm.PointInPolygon (pointInPolygon, underPolygons) where


import Prelude

import Data.Foldable (class Foldable, any)
import Data.Lens ((^.))
import Model.Polygon (class IsPolygon, Polygon, _polyVerts, toPolygon)
import Three.Math.Vector (class Vector, Vector2, toVec2)

-- | test if a 2d point is inside a polygon
foreign import pointInPoly :: Array Vector2 -> Vector2 -> Boolean

pointInPolygon :: forall v w. Vector v => Vector w => Polygon v -> w -> Boolean
pointInPolygon poly v = pointInPoly (toVec2 <$> poly ^. _polyVerts) (toVec2 v)

-- | check if a point is under a list of IsPolygon values
underPolygons :: forall f p v. Functor f => Foldable f => Vector v => IsPolygon p v => f p -> v -> Boolean
underPolygons ps p = any (flip pointInPolygon (toVec2 p)) polys
    where polys :: f (Polygon v)
          polys = toPolygon <$> ps
