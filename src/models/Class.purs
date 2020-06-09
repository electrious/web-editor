module Model.Class where

import Prelude

import Effect (Effect)
import Util (ffi, fpi)
import Model.UUID (PBUUID)
import Model.Racking.Common (LRPB)

-- Class for any value that has a PBUUID value in it
class HasPBUUID a

getUUID :: forall a. HasPBUUID a => a -> PBUUID
getUUID = ffi ["r"] "r.getId()"

setUUID :: forall a. HasPBUUID a => PBUUID -> a -> Effect Unit
setUUID = fpi ["u", "r", ""] "r.setId(u)"


-- Class for any PB value that has x, y and z
class IsPBArrayComp a

getArrayNumber :: forall a. IsPBArrayComp a => a -> Int
getArrayNumber = ffi ["r"] "r.getArrayNumber()"

setArrayNumber :: forall a. IsPBArrayComp a => Int -> a -> Effect Unit
setArrayNumber = fpi ["n", "r", ""] "r.setArrayNumber(n)"

getX :: forall a. IsPBArrayComp a => a -> Number
getX = ffi ["r"] "r.getX()"

getY :: forall a. IsPBArrayComp a => a -> Number
getY = ffi ["r"] "r.getY()"

getZ :: forall a. IsPBArrayComp a => a -> Number
getZ = ffi ["r"] "r.getZ()"

setX :: forall a. IsPBArrayComp a => Number -> a -> Effect Unit
setX = fpi ["x", "r", ""] "r.setX(x)"

setY :: forall a. IsPBArrayComp a => Number -> a -> Effect Unit
setY = fpi ["y", "r", ""] "r.setY(y)"

setZ :: forall a. IsPBArrayComp a => Number -> a -> Effect Unit
setZ = fpi ["z", "r", ""] "r.setZ(z)"

-- Class for any PB value that has length value in it
class HasLength a

getLength :: forall a. HasLength a => a -> Number
getLength = ffi ["r"] "r.getLength()"

setLength :: forall a. HasLength a => Number -> a -> Effect Unit
setLength = fpi ["l", "r", ""] "r.setLength(l)"


class HasPos a

getPos :: forall a. HasPos a => a -> LRPB
getPos = ffi ["r"] "r.getPos()"

setPos :: forall a. HasPos a => LRPB -> a -> Effect Unit
setPos = fpi ["p", "r", ""] "r.setPos(p)"
