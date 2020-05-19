module Model.Racking.FX.Bridge where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter, inch)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)

bridgeWidth :: Meter
bridgeWidth = inch 8.0

newtype Bridge = Bridge {
    id :: UUID,
    x  :: Meter,
    y  :: Meter,
    z  :: Meter,
    arrayNumber :: Int
}

derive instance newtypeBridge :: Newtype Bridge _
derive instance genericBridge :: Generic Bridge _
instance showBridge :: Show Bridge where
    show = genericShow
