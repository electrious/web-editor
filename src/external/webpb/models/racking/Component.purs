module WebPB.Models.Racking.Component where

import Prelude

import Effect (Effect)
import Util (ffi, fpi)
import WebPB.Models.Racking.BX.Block (BlockPB)
import WebPB.Models.Racking.BX.Chassis (ChassisPB)
import WebPB.Models.Racking.FX.Bridge (BridgePB)
import WebPB.Models.Racking.FX.EndCap (EndCapPB)
import WebPB.Models.Racking.FX.Mount (MountPB)
import WebPB.Models.Racking.FX.Skirt (SkirtPB)
import WebPB.Models.Racking.Flash (FlashPB)
import WebPB.Models.Racking.GAF.Hood (HoodPB)
import WebPB.Models.Racking.XR10.Clamp (ClampPB)
import WebPB.Models.Racking.XR10.LFoot (LFootPB)
import WebPB.Models.Racking.XR10.Rail (RailPB)
import WebPB.Models.Racking.XR10.Splice (SplicePB)
import WebPB.Models.Racking.XR10.Stopper (StopperPB)
import WebPB.Models.Racking.XRFlat.QBaseMount (QBaseMountPB)
import WebPB.Models.Racking.XRFlat.SupportRail (SupportRailPB)
import WebPB.Models.Racking.XRFlat.TiltLeg (TiltLegPB)

foreign import data ComponentPB :: Type
foreign import mkComponentPB    :: Effect ComponentPB

foreign import data RdTypeCase  :: Type
foreign import rdTypeNotSet     :: RdTypeCase
foreign import rdTypeRail       :: RdTypeCase
foreign import rdTypeRailFree   :: RdTypeCase
foreign import rdTypeRailFlat   :: RdTypeCase
foreign import rdTypeBallast    :: RdTypeCase
foreign import rdTypeGAF        :: RdTypeCase

getRdTypeCase :: ComponentPB -> RdTypeCase
getRdTypeCase = ffi ["c"] "c.getRdTypeCase()"

getRail :: ComponentPB -> RailComponentPB
getRail = ffi ["c"] "c.getRail()"

setRail :: RailComponentPB -> ComponentPB -> Effect Unit
setRail = fpi ["r", "c", ""] "c.setRail(r)"

getRailFree :: ComponentPB -> RailFreeComponentPB
getRailFree = ffi ["c"] "c.getRailFree()"

setRailFree :: RailFreeComponentPB -> ComponentPB -> Effect Unit
setRailFree = fpi ["r", "c", ""] "c.setRailFree(r)"

getRailFlat :: ComponentPB -> RailFlatComponentPB
getRailFlat = ffi ["c"] "c.getRailFlat()"

setRailFlat :: RailFlatComponentPB -> ComponentPB -> Effect Unit
setRailFlat = fpi ["r", "c", ""] "c.setRailFlat(r)"

getBallast :: ComponentPB -> BallastComponentPB
getBallast = ffi ["c"] "c.getBallast()"

setBallast :: BallastComponentPB -> ComponentPB -> Effect Unit
setBallast = fpi ["b", "c", ""] "c.setBallast(b)"

getGAF :: ComponentPB -> GAFComponentPB
getGAF = ffi ["c"] "c.getGaf()"

setGAF :: GAFComponentPB -> ComponentPB -> Effect Unit
setGAF = fpi ["g", "c", ""] "c.setGaf(g)"


class HasRails a
class HasFlashes a
class HasSplices a
class HasClamps a
class HasStoppers a
class HasArrayNumber a

getArrayNumber :: forall a. HasArrayNumber a => a -> Int
getArrayNumber = ffi ["a"] "a.getArrayNumber()"

setArrayNumber :: forall a. HasArrayNumber a => Int -> a -> Effect Unit
setArrayNumber = fpi ["n", "a", ""] "a.setArrayNumber(n)"

getRails :: forall a. HasRails a => a -> Array RailPB
getRails = ffi ["a"] "a.getRailsList()"

setRails :: forall a. HasRails a => Array RailPB -> a -> Effect Unit
setRails = fpi ["arr", "a", ""] "a.setRailsList(arr)"

getFullRailsNum :: forall a. HasRails a => a -> Int
getFullRailsNum = ffi ["r"] "r.getFullRailsNum()"

setFullRailsNum :: forall a. HasRails a => Int -> a -> Effect Unit
setFullRailsNum = fpi ["n", "r", ""] "r.setFullRailsNum(n)"

getFlashes :: forall a. HasFlashes a => a -> Array FlashPB
getFlashes = ffi ["a"] "a.getFlashesList()"

setFlashes :: forall a. HasFlashes a => Array FlashPB -> a -> Effect Unit
setFlashes = fpi ["arr", "a", ""] "a.setFlashesList(arr)"

getSplices :: forall a. HasSplices a => a -> Array SplicePB
getSplices = ffi ["r"] "r.getSplicesList()"

setSplices :: forall a. HasSplices a => Array SplicePB -> a -> Effect Unit
setSplices = fpi ["a", "r", ""] "r.setSplicesLIst(a)"

getClamps :: forall a. HasClamps a => a -> Array ClampPB
getClamps = ffi ["r"] "r.getClampsList()"

setClamps :: forall a. HasClamps a => Array ClampPB -> a -> Effect Unit
setClamps = fpi ["cs", "r", ""] "r.setClampsList(cs)"

getStoppers :: forall a. HasStoppers a => a -> Array StopperPB
getStoppers = ffi ["r"] "r.getStoppersList()"

setStoppers :: forall a. HasStoppers a => Array StopperPB -> a -> Effect Unit
setStoppers = fpi ["ss", "r", ""] "r.setStoppersList(ss)"


foreign import data RailComponentPB :: Type
foreign import mkRailComponentPB :: Effect RailComponentPB

instance hasArrayNumberRailComponentPB :: HasArrayNumber RailComponentPB
instance hasRailsRailComponentPB       :: HasRails RailComponentPB
instance hasFlashesRailComponentPB     :: HasFlashes RailComponentPB
instance hasSplicesRailComponentPB     :: HasSplices RailComponentPB
instance hasClampsRailComponentPB      :: HasClamps RailComponentPB
instance hasStoppersRailComponentPB    :: HasStoppers RailComponentPB

getLFeet :: RailComponentPB -> Array LFootPB
getLFeet = ffi ["r"] "r.getLfeetList()"

setLFeet :: Array LFootPB -> RailComponentPB -> Effect Unit
setLFeet = fpi ["l", "r", ""] "r.setLfeetList(l)"


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


foreign import data BallastComponentPB :: Type
foreign import mkBallastComponentPB :: Effect BallastComponentPB

instance hasArrayNumberBallastComponentPB :: HasArrayNumber BallastComponentPB

getChassis :: BallastComponentPB -> Array ChassisPB
getChassis = ffi ["b"] "b.getChassisList()"

setChassis :: Array ChassisPB -> BallastComponentPB -> Effect Unit
setChassis = fpi ["cs", "b", ""] "b.setChassisList(cs)"

getBlocks :: BallastComponentPB -> Array BlockPB
getBlocks = ffi ["b"] "b.getBlocksList()"

setBlocks :: Array BlockPB -> BallastComponentPB -> Effect Unit
setBlocks = fpi ["bs", "b", ""] "b.setBlocksList(bs)"


foreign import data GAFComponentPB :: Type
foreign import mkGAFComponentPB :: Effect GAFComponentPB

instance hasArrayNumberGAFComponentPG :: HasArrayNumber GAFComponentPB

getHoods :: GAFComponentPB -> Array HoodPB
getHoods = ffi ["g"] "g.getHoodsList()"

setHoods :: Array HoodPB -> GAFComponentPB -> Effect Unit
setHoods = fpi ["hs", "g", ""] "g.setHoodsList(hs)"
