module WebPB.Models.Racking.FX.EndCap where

import Prelude

import Effect (Effect)
import Util (ffi, fpi)
import WebPB.Class (class HasPBUUID, class HasPos, class IsPBArrayComp)
import WebPB.UUID (PBUUID)
  
foreign import data EndCapPB :: Type
foreign import mkEndCapPB :: Effect EndCapPB

instance hasPBUUIdEndCapPB :: HasPBUUID EndCapPB
instance isPBArrayCompEndCapPB :: IsPBArrayComp EndCapPB
instance hasPosEndCapPB :: HasPos EndCapPB

getSkirt :: EndCapPB -> PBUUID
getSkirt = ffi ["e"] "e.getSkirt()"

setSkirt :: PBUUID -> EndCapPB -> Effect Unit
setSkirt = fpi ["s", "e", ""] "e.setSkirt(s)"
