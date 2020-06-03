module WebPB.Models.Racking.XR10.Splice where

import Prelude

import Effect (Effect)
import Util (ffi, fpi)
import WebPB.Class (class HasPBUUID, class IsPBArrayComp)
import WebPB.UUID (PBUUID)

foreign import data SplicePB :: Type
foreign import mkSplicePB :: Effect SplicePB

instance hasPBUUIDSplicePB :: HasPBUUID SplicePB
instance isPBArrayCompSplicePB :: IsPBArrayComp SplicePB

getRails :: SplicePB -> Array PBUUID
getRails = ffi ["r"] "r.getRailsList()"

setRails :: Array PBUUID -> SplicePB -> Effect Unit
setRails = fpi ["a", "r", ""] "r.setRailsList(a)"
