module WebPB.Models.Racking.BX.Chassis where

import Prelude

import Effect (Effect)
import Util (ffi, fpi)
import WebPB.Class (class HasLength, class HasPBUUID, class IsPBArrayComp)

foreign import data ChassisKind :: Type
foreign import chassisKindInvalid :: ChassisKind
foreign import chassisKindNorth :: ChassisKind
foreign import chassisKindSouth :: ChassisKind

foreign import data ChassisPB :: Type
foreign import mkChassisPB :: Effect ChassisPB

instance hasPBUUIDChassisPB :: HasPBUUID ChassisPB
instance isPBArrayCompChassisPB :: IsPBArrayComp ChassisPB
instance hasLengthChassisPB :: HasLength ChassisPB

getTilt :: ChassisPB -> Number
getTilt = ffi ["c"] "c.getTilt()"

setTilt :: Number -> ChassisPB -> Effect Unit
setTilt = ffi ["t", "c", ""] "c.setTilt(t)"

getKind :: ChassisPB -> ChassisKind
getKind = ffi ["c"] "c.getKind()"

setKind :: ChassisKind -> ChassisPB -> Effect Unit
setKind = fpi ["k", "c", ""] "c.setKind(k)"