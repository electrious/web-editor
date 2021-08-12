module Model.Racking.GAF.Hood where

import Prelude

import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens (view, (^.), (.~))
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _length, _width, _x, _y, _z)
import Model.ArrayComponent (class ArrayComponent)
import Model.RoofComponent (class RoofComponent)

data HoodType = HoodTop
              | HoodBottom
derive instance eqHoodType :: Eq HoodType
derive instance ordHoodType :: Ord HoodType
derive instance genericHoodType :: Generic HoodType _
instance showHoodType :: Show HoodType where
    show = genericShow

newtype Hood = Hood {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    length      :: Meter,
    type        :: HoodType
}

derive instance newtypeHood :: Newtype Hood _
derive instance genericHood :: Generic Hood _
instance showHood :: Show Hood where
    show = genericShow
instance roofComponentHood :: RoofComponent Hood where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size h = def # _width  .~ h ^. _length
                 # _height .~ meter 0.1
instance arrayComponent :: ArrayComponent Hood where
    arrayNumber = view _arrayNumber
