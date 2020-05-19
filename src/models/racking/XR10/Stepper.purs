module Model.Racking.XR10.Stepper where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Model.Racking.Common (RackPos)

newtype Stepper = Stepper {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    type        :: RackPos
}

derive instance newtypeStepper :: Newtype Stepper _
derive instance genericStepper :: Generic Stepper _
instance showStepper :: Show Stepper where
    show = genericShow
