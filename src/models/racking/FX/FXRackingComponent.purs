module Model.Racking.FX.FXRackingComponent where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Model.Racking.FX.Bridge (Bridge)
import Model.Racking.FX.EndCap (EndCap)
import Model.Racking.FX.Mount (Mount)
import Model.Racking.FX.Skirt (Skirt)
import Model.Racking.Flash (Flash)

newtype FXRackingComponent = FXRackingComponent {
    arrayNumber  :: Int,
    flashes      :: Array Flash,
    mounts       :: Array Mount,
    bridges      :: Array Bridge,
    skirts       :: Array Skirt,
    leftEndCaps  :: Array EndCap,
    rightEndCaps :: Array EndCap
}

derive instance newtypeFXRackingComponent :: Newtype FXRackingComponent _
derive instance genericFXRackingComponent :: Generic FXRackingComponent _
instance showFXRackingComponent :: Show FXRackingComponent where
    show = genericShow

newtype FXRackingNumbers = FXRackingNumbers {
    flashes      :: Int,
    mounts       :: Int,
    bridges      :: Int,
    skirts       :: Int,
    leftEndCaps  :: Int,
    rightEndCaps :: Int
}

derive instance newtypeFXRackingNumbers :: Newtype FXRackingNumbers _
derive instance genericFXRackingNumbers :: Generic FXRackingNumbers _
instance showFXRackingNumbers :: Show FXRackingNumbers where
    show = genericShow
