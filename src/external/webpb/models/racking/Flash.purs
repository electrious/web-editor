module WebPB.Models.Racking.Flash where

import Prelude

import Effect (Effect)
import Util (ffi, fpi)
import WebPB.Class (class HasPBUUID, class IsPBArrayComp)
import WebPB.UUID (PBUUID)

foreign import data FlashPB :: Type
foreign import mkFlashPB :: Effect FlashPB

instance hasPBUUIDFlashPB :: HasPBUUID FlashPB
instance isPBArrayCompFlashPB :: IsPBArrayComp FlashPB

getRafterId :: FlashPB -> PBUUID
getRafterId = ffi ["r"] "r.getRafterId()"

setRafterId :: PBUUID -> FlashPB -> Effect Unit
setRafterId = fpi ["u", "r", ""] "r.setRafterId(u)"

getClampTarget :: FlashPB -> Number
getClampTarget = ffi ["r"] "r.getClampTarget()"

setClampTarget :: Number -> FlashPB -> Effect Unit
setClampTarget = fpi ["t", "r", ""] "r.setClampTarget(t)"
