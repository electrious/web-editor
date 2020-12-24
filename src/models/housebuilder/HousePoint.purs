module Model.HouseEditor.HousePoint where

import Prelude

import Three.Math.Vector (Vector3)


data HousePoint = GutterPoint Vector3
                | RidgePoint Vector3

derive instance eqHousePoint :: Eq HousePoint

pointPos :: HousePoint -> Vector3
pointPos (GutterPoint p) = p
pointPos (RidgePoint p)  = p

