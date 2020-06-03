module WebPB.Models.Racking.FX.Skirt where

import Effect (Effect)
import WebPB.Class (class HasLength, class HasPBUUID, class IsPBArrayComp)

foreign import data SkirtPB :: Type
foreign import mkSkirtPB :: Effect SkirtPB

instance hasPBUUIDSkirtPB :: HasPBUUID SkirtPB
instance isPBArrayCompSkirtPB :: IsPBArrayComp SkirtPB
instance hasLengthSkirtPB :: HasLength SkirtPB