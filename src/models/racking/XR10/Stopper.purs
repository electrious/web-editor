module Model.Racking.XR10.Stopper where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Model.Racking.Common (RackPos)

newtype Stopper = Stopper {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    type        :: RackPos
}

derive instance newtypeStopper :: Newtype Stopper _
derive instance genericStopper :: Generic Stopper _
instance showStopper :: Show Stopper where
    show = genericShow
