module Model.Racking.XR10.Rail where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter, feetInch)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)

railLength :: Meter
railLength = feetInch 14.0 0.0

newtype Rail = Rail {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    length      :: Meter,
    panelIds    :: Array UUID
}

derive instance newtypeRail :: Newtype Rail _
derive instance genericRail :: Generic Rail _
instance showRail :: Show Rail where
    show = genericShow
