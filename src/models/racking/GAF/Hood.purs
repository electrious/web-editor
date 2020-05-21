module Model.Racking.GAF.Hood where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)

newtype Hood = Hood {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    length      :: Meter
}

derive instance newtypeHood :: Newtype Hood _
derive instance genericHood :: Generic Hood _
instance showHood :: Show Hood where
    show = genericShow
