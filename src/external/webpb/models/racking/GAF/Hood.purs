module WebPB.Models.Racking.GAF.Hood where

import Prelude

import Effect (Effect)
import Util (ffi, fpi)
import WebPB.Class (class HasLength, class HasPBUUID, class IsPBArrayComp)

foreign import data HoodKind :: Type
foreign import hoodKindInvalid :: HoodKind
foreign import hoodKindTop :: HoodKind
foreign import hoodKindBottom :: HoodKind

foreign import data HoodPB :: Type
foreign import mkHoodPB :: Effect HoodPB

instance hasPBUUIDHoodPB :: HasPBUUID HoodPB
instance isPBArrayCompHoodPB :: IsPBArrayComp HoodPB
instance hasLengthHoodPB :: HasLength HoodPB

getKind :: HoodPB -> HoodKind
getKind = ffi ["h"] "h.getKind()"

setKind :: HoodKind -> HoodPB -> Effect Unit
setKind = fpi ["k", "h", ""] "h.setKind(k)"
