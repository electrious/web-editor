module Editor.Polygon where

import Data.Newtype (class Newtype)
import Three.Math.Vector (Vector2)

newtype Polygon = Polygon (Array Vector2)

derive instance newtypePolygon :: Newtype Polygon _
