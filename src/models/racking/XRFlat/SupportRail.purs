module Model.Racking.XRFlat.SupportRail where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)

newtype SupportRail = SupportRail {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    length      :: Meter
}

derive instance newtypeSupportRail :: Newtype SupportRail _
derive instance genericSupportRail :: Generic SupportRail _
instance showSupportRail :: Show SupportRail where
    show = genericShow
