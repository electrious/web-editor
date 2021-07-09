module Model.Racking.XRFlat.XRFlatRackingComponent where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Newtype (class Newtype)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Model.Racking.Class (class HasArrayNumber, class HasClamps, class HasRails, class HasSplices, class HasStoppers, getArrayNumber, getClamps, getFullRailsNum, getRails, getSplices, getStoppers)
import Model.Racking.XR10.Clamp (Clamp)
import Model.Racking.XR10.Rail (Rail)
import Model.Racking.XR10.Splice (Splice)
import Model.Racking.XR10.Stopper (Stopper)
import Model.Racking.XRFlat.QBaseMount (QBaseMount, QBaseMountPB)
import Model.Racking.XRFlat.SupportRail (SupportRail, SupportRailPB)
import Model.Racking.XRFlat.TiltLeg (TiltLeg, TiltLegPB)

foreign import data RailFlatComponentPB :: Type
foreign import mkRailFlatComponentPB :: Effect RailFlatComponentPB

instance hasArrayNumberRailFlatComponentPB :: HasArrayNumber RailFlatComponentPB
instance hasRailsRailFlatComponentPB :: HasRails RailFlatComponentPB
instance hasSplicesRailFlatComponentPB :: HasSplices RailFlatComponentPB
instance hasClampsRailFlatComponentPB :: HasClamps RailFlatComponentPB
instance hasStoppersRailFlatComponentPB :: HasStoppers RailFlatComponentPB

foreign import getSupportRails :: RailFlatComponentPB -> Array SupportRailPB
foreign import setSupportRails :: Array SupportRailPB -> RailFlatComponentPB -> Effect Unit
foreign import getQBaseMounts :: RailFlatComponentPB -> Array QBaseMountPB
foreign import setQBaseMounts :: Array QBaseMountPB -> RailFlatComponentPB -> Effect Unit
foreign import getTiltLegs :: RailFlatComponentPB -> Array TiltLegPB
foreign import setTiltLegs :: Array TiltLegPB -> RailFlatComponentPB -> Effect Unit


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
instance protoDecodableXRFlatRackingComponent :: ProtoDecodable XRFlatRackingComponent RailFlatComponentPB where
    fromProto c = XRFlatRackingComponent {
        arrayNumber  : getArrayNumber c,
        rails        : fromProto <$> getRails c,
        railsNum     : getFullRailsNum c,
        splices      : fromProto <$> getSplices c,
        clamps       : fromProto <$> getClamps c,
        stoppers     : fromProto <$> getStoppers c,
        supportRails : fromProto <$> getSupportRails c,
        baseMounts   : fromProto <$> getQBaseMounts c,
        tiltLegs     : fromProto <$> getTiltLegs c
    }

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
