module SmartHouse.ShadeOption where

import Data.Bounded (class Bounded, class Ord)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Ord.Generic (genericCompare)
import Data.Show (class Show)

data ShadeOption = NoShade
                 | LightShade
                 | ModerateShade
                 | HeavyShade

derive instance eqShadeOption :: Eq ShadeOption
derive instance genericShadeOption :: Generic ShadeOption _

instance showShadeOption :: Show ShadeOption where
    show NoShade       = "No Shade"
    show LightShade    = "Light Shade"
    show ModerateShade = "Moderate Shade"
    show HeavyShade    = "Heavy Shade"

instance ordShadeOption :: Ord ShadeOption where
    compare = genericCompare
instance enumShadeOption :: Enum ShadeOption where
    succ = genericSucc
    pred = genericPred
instance boundedShadeOption :: Bounded ShadeOption where
    top    = genericTop
    bottom = genericBottom
instance boundedEnumShadeOption :: BoundedEnum ShadeOption where
    cardinality = genericCardinality
    toEnum      = genericToEnum
    fromEnum    = genericFromEnum
