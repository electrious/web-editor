module Model.Racking.FX.FXRackingComponent where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Model.Racking.Class (class HasArrayNumber, class HasFlashes, getArrayNumber, getFlashes)
import Model.Racking.FX.Bridge (Bridge, BridgePB)
import Model.Racking.FX.EndCap (EndCap, EndCapPB)
import Model.Racking.FX.Mount (Mount, MountPB)
import Model.Racking.FX.Skirt (Skirt, SkirtPB)
import Model.Racking.Flash (Flash)
import Util (ffi, fpi)

foreign import data RailFreeComponentPB :: Type
foreign import mkRailFreeComponentPB :: Effect RailFreeComponentPB

instance hasArrayNumberRailFreeComponentPB :: HasArrayNumber RailFreeComponentPB
instance hasFlashesRailFreeComponentPB :: HasFlashes RailFreeComponentPB

getMounts :: RailFreeComponentPB -> Array MountPB
getMounts = ffi ["r"] "r.getMountsList()"

setMounts :: Array MountPB -> RailFreeComponentPB -> Effect Unit
setMounts = fpi ["ms", "r", ""] "r.setMountsList(ms)"

getBridges :: RailFreeComponentPB -> Array BridgePB
getBridges = ffi ["r"] "r.getBrigdesList()"

setBridges :: Array BridgePB -> RailFreeComponentPB -> Effect Unit
setBridges = fpi ["bs", "r", ""] "r.setBridgesList(bs)"

getSkirts :: RailFreeComponentPB -> Array SkirtPB
getSkirts = ffi ["r"] "r.getSkirtsList()"

setSkirts :: Array SkirtPB -> RailFreeComponentPB -> Effect Unit
setSkirts = fpi ["ss", "r", ""] "r.setSkirtsList(ss)"

getLeftCaps :: RailFreeComponentPB -> Array EndCapPB
getLeftCaps = ffi ["r"] "r.getLeftCapsList()"

setLeftCaps :: Array EndCapPB -> RailFreeComponentPB -> Effect Unit
setLeftCaps = fpi ["cs", "r", ""] "r.setLeftCapsList(cs)"

getRightCaps :: RailFreeComponentPB -> Array EndCapPB
getRightCaps = ffi ["r"] "r.getRightCapsList()"

setRightCaps :: Array EndCapPB -> RailFreeComponentPB -> Effect Unit
setRightCaps = fpi ["cs", "r", ""] "r.setRightCapsList(cs)"

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
instance protoDecodableRXRackingComponent :: ProtoDecodable FXRackingComponent RailFreeComponentPB where
    fromProto c = FXRackingComponent {
        arrayNumber  : getArrayNumber c,
        flashes      : fromProto <$> getFlashes c,
        mounts       : fromProto <$> getMounts c,
        bridges      : fromProto <$> getBridges c,
        skirts       : fromProto <$> getSkirts c,
        leftEndCaps  : fromProto <$> getLeftCaps c,
        rightEndCaps : fromProto <$> getRightCaps c
    }

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
