module Model.Racking.XRFlat.XRFlatRackingComponent where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Model.Racking.XR10.Rail (Rail)
import Model.Racking.XR10.Clamp (Clamp)
import Model.Racking.XR10.Splice (Splice)
import Model.Racking.XR10.Stopper (Stopper)
import Model.Racking.XRFlat.QBaseMount (QBaseMount)
import Model.Racking.XRFlat.SupportRail (SupportRail)
import Model.Racking.XRFlat.TiltLeg (TiltLeg)

newtype XRFlatRackingComponent = XRFlatRackingComponent {
    arrayNumber  :: Int,
    rails        :: Array Rail,
    railsNum     :: Int,
    splices      :: Array Splice,
    clamps       :: Array Clamp,
    stoppers     :: Array Stopper,
    supportRails :: Array SupportRail,
    baseMounts   :: Array QBaseMount,
    tiltLegs     :: Array TiltLeg
}

derive instance newtypeXRFlatRackingComponent :: Newtype XRFlatRackingComponent _
derive instance genericXRFlatRackingComponent :: Generic XRFlatRackingComponent _
instance showXRFlatRackingComponent :: Show XRFlatRackingComponent where
    show = genericShow


newtype XRFlatRackingNumbers = XRFlatRackingNumbers {
    rails        :: Int,
    splices      :: Int,
    clamps       :: Int,
    stoppers     :: Int,
    supportRails :: Int,
    baseMounts   :: Int,
    tiltLegs     :: Int
}

derive instance newtypeXRFlatRackingNumbers :: Newtype XRFlatRackingNumbers _
derive instance genericXRFlatRackingNumbers :: Generic XRFlatRackingNumbers _
instance showXRFlatRackingNumbers :: Show XRFlatRackingNumbers where
    show = genericShow
