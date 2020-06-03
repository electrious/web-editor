module WebPB.Models.Racking.FX.Bridge where

import Effect (Effect)
import WebPB.Class (class HasPBUUID, class IsPBArrayComp)

foreign import data BridgePB :: Type
foreign import mkBridgePB :: Effect BridgePB

instance hasPBUUIdBridgePB :: HasPBUUID BridgePB
instance isPBArrayCompBridgePB :: IsPBArrayComp BridgePB
