module Model.Racking.FX.Skirt where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)

newtype Skirt = Skirt {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    length      :: Meter
}

derive instance newtypeSkirt :: Newtype Skirt _
derive instance genericSKirt :: Generic Skirt _
instance showSkirt :: Show Skirt where
    show = genericShow
