module Model.HouseBuilder.FloorPlan where

import Prelude

import Data.Meter (Meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.Lenses (_id)
import Model.Polygon (Polygon)
import Model.UUID (class HasUUID)

-- | a FloorPlan represent a house part with 2D polygon and height
newtype FloorPlan = FloorPlan {
    id      :: UUID,
    polygon :: Polygon,
    height  :: Meter
}

derive instance newtypeFloorPlan :: Newtype FloorPlan _
derive instance eqFloorPlan :: Eq FloorPlan
instance hasUUIDFloorPlan :: HasUUID FloorPlan where
    idLens = _id


-- | operations applied to FloorPlans
data FloorPlanOp = FPOCreate FloorPlan
                 | FPODelete UUID
                 | FPOUpdate FloorPlan

derive instance eqFloorPlanOp :: Eq FloorPlanOp
