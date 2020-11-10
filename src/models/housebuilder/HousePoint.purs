module Model.HouseEditor.HousePoint where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Three.Math.Vector (Vector3)

data HousePoint = GutterPoint Vector3
                | RidgePoint Vector3

derive instance genericHousePoint :: Generic HousePoint _
derive instance eqHousePoint :: Eq HousePoint
instance showHousePoint :: Show HousePoint where
    show = genericShow

gutterPoint :: Vector3 -> HousePoint
gutterPoint = GutterPoint

ridgePoint :: Vector3 -> HousePoint
ridgePoint = RidgePoint

pointPos :: HousePoint -> Vector3
pointPos (GutterPoint p) = p
pointPos (RidgePoint p) = p