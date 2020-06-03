module WebPB.Models.Racking.XR10.LFoot where

import Prelude

import Effect (Effect)
import Util (ffi, fpi)
import WebPB.Class (class HasPBUUID, class IsPBArrayComp)
import WebPB.UUID (PBUUID)

foreign import data LFootPB :: Type
foreign import mkLFootPB :: Effect LFootPB

instance hasPBUUIDLFootPB :: HasPBUUID LFootPB
instance isPBArrayCompLFootPB :: IsPBArrayComp LFootPB

getFlashId :: LFootPB -> PBUUID
getFlashId = ffi ["r"] "r.getFlash()"

setFlashId :: PBUUID -> LFootPB -> Effect Unit
setFlashId = fpi ["u", "r", ""] "r.setFlash(u)"
