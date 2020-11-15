module Model.HouseEditor.HousePoint where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Three.Math.Vector (Vector3)

data GutterPointType = GPPredicted  -- a predicted point that might be used for next gutter point
                     | GPLocked     -- an already locked point
                     | GPReused     -- a locked point that user might want to reuse

derive instance genericGutterPointType :: Generic GutterPointType _
derive instance eqGutterPointType :: Eq GutterPointType
instance showGutterPointType :: Show GutterPointType where
    show = genericShow


data HousePoint = GutterPoint GutterPointType Vector3
                | RidgePoint Vector3

derive instance genericHousePoint :: Generic HousePoint _
derive instance eqHousePoint :: Eq HousePoint
instance showHousePoint :: Show HousePoint where
    show = genericShow

gutterPoint :: GutterPointType -> Vector3 -> HousePoint
gutterPoint t p = GutterPoint t p

ridgePoint :: Vector3 -> HousePoint
ridgePoint = RidgePoint

pointPos :: HousePoint -> Vector3
pointPos (GutterPoint _ p) = p
pointPos (RidgePoint p) = p

setGutterType :: GutterPointType -> HousePoint -> HousePoint
setGutterType t (GutterPoint _ p) = GutterPoint t p
setGutterType _ p@(RidgePoint _)  = p

