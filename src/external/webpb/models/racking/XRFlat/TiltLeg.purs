module WebPB.Models.Racking.XRFlat.TiltLeg where

import Prelude

import Effect (Effect)
import Util (ffi, fpi)
import WebPB.Class (class HasLength, class HasPBUUID, class IsPBArrayComp)

foreign import data TiltLegKind :: Type
foreign import tiltKindInvalid :: TiltLegKind
foreign import tiltKindNorth :: TiltLegKind
foreign import tiltKindSouth :: TiltLegKind

foreign import data TiltLegPB :: Type
foreign import mkTiltLegPB :: Effect TiltLegPB

instance hasPBUUIDTiltLegPB :: HasPBUUID TiltLegPB
instance isPBArrayCompTiltLegPB :: IsPBArrayComp TiltLegPB
instance hasLengthTiltLegPB :: HasLength TiltLegPB

getTilt :: TiltLegPB -> Number
getTilt = ffi ["t"] "t.getTilt()"

setTilt :: Number -> TiltLegPB -> Effect Unit
setTilt = fpi ["l", "t", ""] "t.setTilt(l)"

getKind :: TiltLegPB -> TiltLegKind
getKind = ffi ["t"] "t.getKind()"

setKind :: TiltLegKind -> TiltLegPB -> Effect Unit
setKind = fpi ["k", "t", ""] "t.setKind(k)"