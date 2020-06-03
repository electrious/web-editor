module WebPB.Models.Racking.XR10.Stopper where

import Effect (Effect)
import WebPB.Class (class HasPBUUID, class HasPos, class IsPBArrayComp)

foreign import data StopperPB :: Type
foreign import mkStopperPB :: Effect StopperPB

instance hasPBUUIDStopperPB :: HasPBUUID StopperPB
instance isPBArrayCompStopperPB :: IsPBArrayComp StopperPB
instance hasPosStopperPB :: HasPos StopperPB
