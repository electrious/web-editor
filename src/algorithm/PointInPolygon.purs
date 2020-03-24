module Algorithm.PointInPolygon where

import Models.RoofPlate (Polygon)
import Three.Math.Vector (Vector2)

-- | test if a 2d point is inside a polygon
foreign import pointInPolygon :: Polygon -> Vector2 -> Boolean
