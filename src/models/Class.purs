module Model.Class where

import Prelude

import Effect (Effect)
import Model.UUID (PBUUID)
import Model.Racking.Common (LRPB)

-- Class for any value that has a PBUUID value in it
class HasPBUUID :: forall k. k -> Constraint
class HasPBUUID a

foreign import getUUID :: forall a. HasPBUUID a => a -> PBUUID
foreign import setUUID :: forall a. HasPBUUID a => PBUUID -> a -> Effect Unit

-- Class for any PB value that has x, y and z
class IsPBArrayComp :: forall k. k -> Constraint
class IsPBArrayComp a

foreign import getArrayNumber :: forall a. IsPBArrayComp a => a -> Int
foreign import setArrayNumber :: forall a. IsPBArrayComp a => Int -> a -> Effect Unit
foreign import getX :: forall a. IsPBArrayComp a => a -> Number
foreign import getY :: forall a. IsPBArrayComp a => a -> Number
foreign import getZ :: forall a. IsPBArrayComp a => a -> Number
foreign import setX :: forall a. IsPBArrayComp a => Number -> a -> Effect Unit
foreign import setY :: forall a. IsPBArrayComp a => Number -> a -> Effect Unit
foreign import setZ :: forall a. IsPBArrayComp a => Number -> a -> Effect Unit

-- Class for any PB value that has length value in it
class HasLength :: forall k. k -> Constraint
class HasLength a

foreign import getLength :: forall a. HasLength a => a -> Number
foreign import setLength :: forall a. HasLength a => Number -> a -> Effect Unit

class HasPos :: forall k. k -> Constraint
class HasPos a

foreign import getPos :: forall a. HasPos a => a -> LRPB
foreign import setPos :: forall a. HasPos a => LRPB -> a -> Effect Unit
