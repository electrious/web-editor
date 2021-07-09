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

class HasRails :: forall k. k -> Constraint
class HasRails a
class HasFlashes :: forall k. k -> Constraint
class HasFlashes a
class HasSplices :: forall k. k -> Constraint
class HasSplices a
class HasClamps :: forall k. k -> Constraint
class HasClamps a
class HasStoppers :: forall k. k -> Constraint
class HasStoppers a
class HasArrayNumber :: forall k. k -> Constraint
class HasArrayNumber a

foreign import getArrayNumber :: forall a. HasArrayNumber a => a -> Int
foreign import setArrayNumber :: forall a. HasArrayNumber a => Int -> a -> Effect Unit
foreign import getRails :: forall a. HasRails a => a -> Array RailPB
foreign import setRails :: forall a. HasRails a => Array RailPB -> a -> Effect Unit
foreign import getFullRailsNum :: forall a. HasRails a => a -> Int
foreign import setFullRailsNum :: forall a. HasRails a => Int -> a -> Effect Unit
foreign import getFlashes :: forall a. HasFlashes a => a -> Array FlashPB
foreign import setFlashes :: forall a. HasFlashes a => Array FlashPB -> a -> Effect Unit
foreign import getSplices :: forall a. HasSplices a => a -> Array SplicePB
foreign import setSplices :: forall a. HasSplices a => Array SplicePB -> a -> Effect Unit
foreign import getClamps :: forall a. HasClamps a => a -> Array ClampPB
foreign import setClamps :: forall a. HasClamps a => Array ClampPB -> a -> Effect Unit
foreign import getStoppers :: forall a. HasStoppers a => a -> Array StopperPB
foreign import setStoppers :: forall a. HasStoppers a => Array StopperPB -> a -> Effect Unit

class HasSpacings :: forall k. k -> Constraint
class HasSpacings a

foreign import getMountSpacing :: forall a. HasSpacings a => a -> MountSpacingPB
foreign import setMountSpacing :: forall a. HasSpacings a => MountSpacingPB -> a -> Effect Unit
foreign import getRafterSpacing :: forall a. HasSpacings a => a -> RafterSpacingPB
foreign import setRafterSpacing :: forall a. HasSpacings a => RafterSpacingPB -> a -> Effect Unit
