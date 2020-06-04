module WebPB.Models.Racking.BX.Block where

import Effect (Effect)
import WebPB.Class (class HasPBUUID, class IsPBArrayComp)

foreign import data BlockPB :: Type
foreign import mkBlockPB :: Effect BlockPB

instance hasPBUUIDBlockPB :: HasPBUUID BlockPB
instance isPBArrayCompBlockPB :: IsPBArrayComp BlockPB
