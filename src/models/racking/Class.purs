module Model.Racking.Class where

import Prelude

import Effect (Effect)
import Model.Racking.Flash (FlashPB)
import Model.Racking.MountSpacing (MountSpacingPB)
import Model.Racking.RafterSpacing (RafterSpacingPB)
import Model.Racking.XR10.Clamp (ClampPB)
import Model.Racking.XR10.Rail (RailPB)
import Model.Racking.XR10.Splice (SplicePB)
import Model.Racking.XR10.Stopper (StopperPB)
import Util (ffi, fpi)

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


class HasSpacings a

getMountSpacing :: forall a. HasSpacings a => a -> MountSpacingPB
getMountSpacing = ffi ["p"] "p.getMountSpace()"

setMountSpacing :: forall a. HasSpacings a => MountSpacingPB -> a -> Effect Unit
setMountSpacing = fpi ["s", "p", ""] "p.setMountSpace(s)"

getRafterSpacing :: forall a. HasSpacings a => a -> RafterSpacingPB
getRafterSpacing = ffi ["p"] "p.getRafterSpace()"

setRafterSpacing :: forall a. HasSpacings a => RafterSpacingPB -> a -> Effect Unit
setRafterSpacing = fpi ["s", "p", ""] "p.setRafterSpace(s)"