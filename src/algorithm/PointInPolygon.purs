module Algorithm.PointInPolygon where

import Model.Roof.RoofPlate (Polygon)
import Three.Math.Vector (Vector2)

-- | test if a 2d point is inside a polygon
foreign import pointInPolygon :: Polygon -> Vector2 -> Boolean
