module Three.Math.Vector  where

import Prelude hiding (add,sub)

import Data.Default (class Default)
import Three.Math.Matrix (Matrix4)
import Util (ffi)

foreign import data Vector2 :: Type
foreign import data Vector3 :: Type

foreign import mkVec2 :: Number -> Number -> Vector2
foreign import mkVec3 :: Number -> Number -> Number -> Vector3

foreign import vEq :: forall a. a -> a -> Boolean

instance eqVec2 :: Eq Vector2 where
  eq = vEq

instance eqVec3 :: Eq Vector3 where
  eq = vEq

instance defaultVector2 :: Default Vector2 where
    def = mkVec2 0.0 0.0

instance defaultVector3 :: Default Vector3 where
    def = mkVec3 0.0 0.0 0.0

class HasX a
class HasY a
class HasZ a

vecX :: forall a. HasX a => a -> Number
vecX = ffi ["vec"] "vec.x"

vecY :: forall a. HasY a => a -> Number
vecY = ffi ["vec"] "vec.y"

vecZ :: forall a. HasZ a => a -> Number
vecZ = ffi ["vec"] "vec.z"

instance hasXVec2 :: HasX Vector2
instance hasYVec2 :: HasY Vector2
instance hasXVec3 :: HasX Vector3
instance hasYVec3 :: HasY Vector3
instance hasZVec3 :: HasZ Vector3

instance showVec2 :: Show Vector2 where
  show = ffi ["vec"] "'(' + vec.x + ', ' + vec.y + ')'"

instance showVec3 :: Show Vector3 where
  show = ffi ["vec"] "'(' + vec.x + ', ' + vec.y + ', ' + vec.z + ')'"


class Vector a

dot :: forall a. Vector a => a -> a -> Number
dot = ffi ["v1", "v2"] "v1.dot(v2)"

length :: forall a. Vector a => a -> Number
length = ffi ["v"] "v.length()"

dist :: forall a. Vector a => a -> a -> Number
dist = ffi ["v1", "v2"] "v1.distanceTo(v2)"

foreign import clone          :: forall a. Vector a => a -> a
foreign import cross          :: forall a. Vector a => a -> a      -> a
foreign import add            :: forall a. Vector a => a -> a      -> a
foreign import addScaled      :: forall a. Vector a => a -> a      -> Number -> a
foreign import sub            :: forall a. Vector a => a -> a      -> a
foreign import multiplyScalar :: forall a. Vector a => a -> Number -> a
foreign import normal         :: forall a. Vector a => a -> a

infixr 5 dot as <.>
infixr 6 add as <+>
infixr 6 sub as <->
infixr 7 multiplyScalar as <**>

instance vecVec2 :: Vector Vector2
instance vecVec3 :: Vector Vector3

foreign import applyMatrix :: Matrix4 -> Vector3 -> Vector3