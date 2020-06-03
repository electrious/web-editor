module WebPB.Models.Racking.FX.Mount where

import Prelude

import Effect (Effect)
import Util (ffi, fpi)
import WebPB.Class (class HasPBUUID, class IsPBArrayComp)
import WebPB.UUID (PBUUID)

foreign import data MountSpacingPB :: Type
foreign import spacing_Invalid :: MountSpacingPB
foreign import spacing_24 :: MountSpacingPB
foreign import spacing_32 :: MountSpacingPB
foreign import spacing_48 :: MountSpacingPB
foreign import spacing_64 :: MountSpacingPB
foreign import spacing_72 :: MountSpacingPB

foreign import data MountPB :: Type
foreign import mkMountPB :: Effect MountPB

instance hasPBUUIdMountPB :: HasPBUUID MountPB
instance isPBArrayCompMountPB :: IsPBArrayComp MountPB

getFlash :: MountPB -> PBUUID
getFlash = ffi ["m"] "m.getFlash()"

setFlash :: PBUUID -> MountPB -> Effect Unit
setFlash = fpi ["f", "m", ""] "m.setFlash(f)"

getClampX :: MountPB -> Number
getClampX = ffi ["m"] "m.getClampX()"

setClampX :: Number -> MountPB -> Effect Unit
setClampX = fpi ["x", "m", ""] "m.setClampX(x)"
