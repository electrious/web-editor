module WebPB.Models.Racking.XRFlat.QBaseMount where

import Prelude

import Effect (Effect)
import Util (ffi, fpi)
import WebPB.Class (class HasPBUUID, class IsPBArrayComp)
  
foreign import data QBaseMountPB :: Type
foreign import mkQBaseMountPB :: Effect QBaseMountPB

instance hasPBUUIDQBaseMountPB :: HasPBUUID QBaseMountPB
instance isPBArrayCompQBaseMountPB :: IsPBArrayComp QBaseMountPB

getHeight :: QBaseMountPB -> Number
getHeight = ffi ["q"] "q.getHeight()"

setHeight :: Number -> QBaseMountPB -> Effect Unit
setHeight = fpi ["h", "q", ""] "q.setHeight(h)"