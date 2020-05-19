module Model.Racking.XR10.XRRackingComponent where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import MOdel.Racking.XR10.Rail (Rail)
import Model.Racking.Flash (Flash)
import Model.Racking.XR10.Clamp (Clamp)
import Model.Racking.XR10.LFoot (LFoot)
import Model.Racking.XR10.Splice (Splice)
import Model.Racking.XR10.Stepper (Stepper)

newtype XRRackingComponent = XRRackingComponent {
    arrayNumber :: Int,
    flashes     :: Array Flash,
    rails       :: Array Rail,
    railsNum    :: Int,
    splices     :: Array Splice,
    lfeet       :: Array LFoot,
    clamps      :: Array Clamp,
    steppers    :: Array Stepper
}

derive instance newtypeXRRackingComponent :: Newtype XRRackingComponent _
derive instance genericXRRackingComponent :: Generic XRRackingComponent _
instance showXRRackingComponent :: Show XRRackingComponent where
    show = genericShow


newtype XRRackingNumbers = XRRackingNumbers {
    flashes  :: Int,
    rails    :: Int,
    splices  :: Int,
    lfeet    :: Int,
    clamps   :: Int,
    steppers :: Int
}

derive instance newtypeXRRackingNumbers :: Newtype XRRackingNumbers _
derive instance genericXRRackingNumbers :: Generic XRRackingNumbers _
instance showXRRackingNumbers :: Show XRRackingNumbers where
    show = genericShow
