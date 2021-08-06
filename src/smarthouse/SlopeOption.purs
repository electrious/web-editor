module SmartHouse.SlopeOption where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Math.Angle (Angle)

data SlopeOption = ActiveRoof Angle
                 | AllRoofs Angle

derive instance Generic SlopeOption _
instance Show SlopeOption where
    show = genericShow


slopeOption :: Angle -> Boolean -> SlopeOption
slopeOption angle true  = AllRoofs angle
slopeOption angle false = ActiveRoof angle
