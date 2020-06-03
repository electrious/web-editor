module WebPB.Models.Racking.XR10.Rail where

import Prelude

import Effect (Effect)
import Util (ffi, fpi)
import WebPB.Class (class HasLength, class HasPBUUID, class IsPBArrayComp)
import WebPB.UUID (PBUUID)

foreign import data RailPB :: Type
foreign import mkRailPB :: Effect RailPB

instance hasPBUUIDRailPB :: HasPBUUID RailPB
instance isPBArrayCompRailPB :: IsPBArrayComp RailPB
instance hasLengthRailPB :: HasLength RailPB

getPanels :: RailPB -> Array PBUUID
getPanels = ffi ["r"] "r.getPanelsList()"

setPanels :: Array PBUUID -> RailPB -> Effect Unit
setPanels = fpi ["a", "r", ""] "r.setPanelsList(a)"
