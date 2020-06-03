module WebPB.Models.Racking.Rafter where


import Effect (Effect)
import WebPB.Class (class HasLength, class HasPBUUID, class IsPBArrayComp)

foreign import data RafterPB :: Type

foreign import mkRafterPB :: Effect RafterPB

instance hasPBUUIDRafterPB :: HasPBUUID RafterPB
instance isPBArrayCompRafterPB :: IsPBArrayComp RafterPB
instance hasLengthRafterPB :: HasLength RafterPB
