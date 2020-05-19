module Model.Racking.FX.Mount where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter, inch, meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)

mountRadius :: Meter
mountRadius = inch 5.0

mountLength :: Meter
mountLength = meter 0.11

mountWidth :: Meter
mountWidth = meter 0.0508

newtype Mount = Mount {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    flashId     :: UUID,
    clampX      :: Meter
}

derive instance newtypeMount :: Newtype Mount _
derive instance genericMount :: Generic Mount _
instance showMount :: Show Mount where
    show = genericShow
