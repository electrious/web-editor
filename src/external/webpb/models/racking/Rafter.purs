module WebPB.Models.Racking.Rafter where


import Effect (Effect)
import WebPB.Class (class HasLength, class HasPBUUID, class IsPBArrayComp)

foreign import data RafterSpacingPB :: Type
foreign import rafterSpacingInvalid :: RafterSpacingPB
foreign import rafterSpacing16 :: RafterSpacingPB
foreign import rafterSpacing24 :: RafterSpacingPB

foreign import data RafterPB :: Type

foreign import mkRafterPB :: Effect RafterPB

instance hasPBUUIDRafterPB :: HasPBUUID RafterPB
instance isPBArrayCompRafterPB :: IsPBArrayComp RafterPB
instance hasLengthRafterPB :: HasLength RafterPB
