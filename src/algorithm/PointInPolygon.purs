module Algorithm.PointInPolygon (pointInPolygon) where

import Model.Polygon (Polygon(..))
import Three.Math.Vector (Vector2)

-- | test if a 2d point is inside a polygon
foreign import pointInPoly :: Array Vector2 -> Vector2 -> Boolean

pointInPolygon :: Polygon -> Vector2 -> Boolean
pointInPolygon (Polygon vs) v = pointInPoly vs v