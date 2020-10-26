module Model.Racking.XR10.XRRackingComponent where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Model.Racking.Class (class HasArrayNumber, class HasClamps, class HasFlashes, class HasRails, class HasSplices, class HasStoppers, getArrayNumber, getClamps, getFlashes, getFullRailsNum, getRails, getSplices, getStoppers)
import Model.Racking.Flash (Flash)
import Model.Racking.XR10.Clamp (Clamp)
import Model.Racking.XR10.LFoot (LFoot, LFootPB)
import Model.Racking.XR10.Rail (Rail)
import Model.Racking.XR10.Splice (Splice)
import Model.Racking.XR10.Stopper (Stopper)

foreign import data RailComponentPB :: Type
foreign import mkRailComponentPB :: Effect RailComponentPB

instance hasArrayNumberRailComponentPB :: HasArrayNumber RailComponentPB
instance hasRailsRailComponentPB       :: HasRails RailComponentPB
instance hasFlashesRailComponentPB     :: HasFlashes RailComponentPB
instance hasSplicesRailComponentPB     :: HasSplices RailComponentPB
instance hasClampsRailComponentPB      :: HasClamps RailComponentPB
instance hasStoppersRailComponentPB    :: HasStoppers RailComponentPB

foreign import getLFeet :: RailComponentPB -> Array LFootPB
foreign import setLFeet :: Array LFootPB -> RailComponentPB -> Effect Unit


newtype XRRackingComponent = XRRackingComponent {
    arrayNumber :: Int,
    flashes     :: Array Flash,
    rails       :: Array Rail,
    railsNum    :: Int,
    splices     :: Array Splice,
    lfeet       :: Array LFoot,
    clamps      :: Array Clamp,
    stoppers    :: Array Stopper
}

derive instance newtypeXRRackingComponent :: Newtype XRRackingComponent _
derive instance genericXRRackingComponent :: Generic XRRackingComponent _
instance showXRRackingComponent :: Show XRRackingComponent where
    show = genericShow
instance protoDecodableXRRackingComponent :: ProtoDecodable XRRackingComponent RailComponentPB where
    fromProto c = XRRackingComponent {
        arrayNumber : getArrayNumber c,
        flashes     : fromProto <$> getFlashes c,
        rails       : fromProto <$> getRails c,
        railsNum    : getFullRailsNum c,
        splices     : fromProto <$> getSplices c,
        lfeet       : fromProto <$> getLFeet c,
        clamps      : fromProto <$> getClamps c,
        stoppers    : fromProto <$> getStoppers c
    }

newtype XRRackingNumbers = XRRackingNumbers {
    flashes  :: Int,
    rails    :: Int,
    splices  :: Int,
    lfeet    :: Int,
    clamps   :: Int,
    stoppers :: Int
}

derive instance newtypeXRRackingNumbers :: Newtype XRRackingNumbers _
derive instance genericXRRackingNumbers :: Generic XRRackingNumbers _
instance showXRRackingNumbers :: Show XRRackingNumbers where
    show = genericShow
