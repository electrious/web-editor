module WebPB.Models.Racking.XR10.Clamp where

import Prelude

import Effect (Effect)
import Util (ffi, fpi)
import WebPB.Class (class HasPBUUID, class HasPos, class IsPBArrayComp)

foreign import data ClampKindPB :: Type
foreign import kind_Invalid :: ClampKindPB
foreign import kind_Middle :: ClampKindPB
foreign import kind_End :: ClampKindPB

foreign import data ClampPB :: Type
foreign import mkClampPB :: Effect ClampPB

instance hasPBUUIDClampPB :: HasPBUUID ClampPB
instance isPBArrayCompClampPB :: IsPBArrayComp ClampPB
instance hasPosClampPB :: HasPos ClampPB

getKind :: ClampPB -> ClampKindPB
getKind = ffi ["r"] "r.getKind()"

setKind :: ClampKindPB -> ClampPB -> Effect Unit
setKind = fpi ["k", "r", ""] "r.setKind(k)"

