module Model.HouseBuilder.FloorPlan where

import Data.Meter (Meter)
import Data.Newtype (class Newtype)
import Model.Polygon (Polygon)

-- | a FloorPlan represent a house part with 2D polygon and height
newtype FloorPlan = FloorPlan {
    polygon :: Polygon,
    height  :: Meter
}

derive instance newtypeFloorPlan :: Newtype FloorPlan _
