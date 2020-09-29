module Model.Racking.XRFlat.XRFlatRackingComponent where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
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
import Util (ffi, fpi)


foreign import data RailFlatComponentPB :: Type
foreign import mkRailFlatComponentPB :: Effect RailFlatComponentPB

instance hasArrayNumberRailFlatComponentPB :: HasArrayNumber RailFlatComponentPB
instance hasRailsRailFlatComponentPB :: HasRails RailFlatComponentPB
instance hasSplicesRailFlatComponentPB :: HasSplices RailFlatComponentPB
instance hasClampsRailFlatComponentPB :: HasClamps RailFlatComponentPB
instance hasStoppersRailFlatComponentPB :: HasStoppers RailFlatComponentPB

getSupportRails :: RailFlatComponentPB -> Array SupportRailPB
getSupportRails = ffi ["r"] "r.getSupportRailsList()"

setSupportRails :: Array SupportRailPB -> RailFlatComponentPB -> Effect Unit
setSupportRails = fpi ["ss", "r", ""] "r.setSupportRailsList(ss)"

getQBaseMounts :: RailFlatComponentPB -> Array QBaseMountPB
getQBaseMounts = ffi ["r"] "r.getQbaseMountsList()"

setQBaseMounts :: Array QBaseMountPB -> RailFlatComponentPB -> Effect Unit
setQBaseMounts = fpi ["qs", "r", ""] "r.setQbaseMountsList(qs)"

getTiltLegs :: RailFlatComponentPB -> Array TiltLegPB
getTiltLegs = ffi ["r"] "r.getTiltLegsList()"

setTiltLegs :: Array TiltLegPB -> RailFlatComponentPB -> Effect Unit
setTiltLegs = fpi ["ts", "r", ""] "r.setTiltLegsList(ts)"


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
