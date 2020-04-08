module Three.Math.Vector (
  Vector2, Vector3, mkVec2, mkVec3, class HasX, vecX,
  class HasY, vecY, class HasZ, vecZ,
  class Vector, dot, (<.>), (<+>), (<->), length, dist, cross, add, addScaled, sub, normal,
  multiplyScalar, applyMatrix
  ) where

import Prelude

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

getX :: forall a.a -> Number
getX = ffi ["vec"] "vec.x"

getY :: forall a.a -> Number
getY = ffi ["vec"] "vec.y"

getZ :: forall a.a -> Number
getZ = ffi ["vec"] "vec.z"

class HasX a where
  vecX :: a -> Number

class HasY a where
  vecY :: a -> Number

class HasZ a where
  vecZ :: a -> Number

instance hasXVec2 :: HasX Vector2 where
  vecX = getX

instance hasYVec2 :: HasY Vector2 where
  vecY = getY

instance hasXVec3 :: HasX Vector3 where
  vecX = getX

instance hasYVec3 :: HasY Vector3 where
  vecY = getY

instance hasZVec3 :: HasZ Vector3 where
  vecZ = getZ

instance showVec2 :: Show Vector2 where
  show = ffi ["vec"] "'(' + vec.x + ', ' + vec.y + ')'"

instance showVec3 :: Show Vector3 where
  show = ffi ["vec"] "'(' + vec.x + ', ' + vec.y + ', ' + vec.z + ')'"


vDot :: forall a. a -> a -> Number
vDot = ffi ["v1", "v2"] "v1.dot(v2)"

vLength :: forall a. a -> Number
vLength = ffi ["v"] "v.length()"

vDist :: forall a. a -> a -> Number
vDist = ffi ["v1", "v2"] "v1.distanceTo(v2)"

foreign import vCross :: forall a. a -> a -> a
foreign import vAdd :: forall a. a -> a -> a
foreign import vAddScaled :: forall a. a -> a -> Number -> a
foreign import vSub :: forall a. a -> a -> a
foreign import vMultiplyScalar :: forall a. a -> Number -> a
foreign import vNormal :: forall a. a -> a

class Vector a where
  dot :: a -> a -> Number
  length :: a -> Number
  dist :: a -> a -> Number
  cross :: a -> a -> a
  add :: a -> a -> a
  addScaled :: a -> a -> Number -> a
  sub :: a -> a -> a
  multiplyScalar :: a -> Number -> a
  normal :: a -> a

infixr 5 dot as <.>
infixr 6 add as <+>
infixr 6 sub as <->


instance vecVec2 :: Vector Vector2 where
  dot = vDot
  length = vLength
  dist = vDist
  cross = vCross
  add = vAdd
  addScaled = vAddScaled
  sub = vSub
  multiplyScalar = vMultiplyScalar
  normal = vNormal

instance vecVec3 :: Vector Vector3 where
  dot = vDot
  length = vLength
  dist = vDist
  cross = vCross
  add = vAdd
  addScaled = vAddScaled
  sub = vSub
  multiplyScalar = vMultiplyScalar
  normal = vNormal

foreign import applyMatrix :: Matrix4 -> Vector3 -> Vector3