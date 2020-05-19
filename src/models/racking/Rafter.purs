module Model.Racking.Rafter where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)

newtype Rafter = Rafter {
    id     :: UUID,
    x      :: Meter,
    y      :: Meter,
    z      :: Meter,
    length :: Meter
}

derive instance newtypeRafter :: Newtype Rafter _
derive instance genericRafter :: Generic Rafter _
instance showRafter :: Show Rafter where
    show = genericShow
