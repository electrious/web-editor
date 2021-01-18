module Three.Math.Vector  where

import Prelude hiding (add,sub)

import Data.Default (class Default)
import Three.Math.Matrix (Matrix4)

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

foreign import vecX :: forall a. HasX a => a -> Number
foreign import vecY :: forall a. HasY a => a -> Number
foreign import vecZ :: forall a. HasZ a => a -> Number

instance hasXVec2 :: HasX Vector2
instance hasYVec2 :: HasY Vector2
instance hasXVec3 :: HasX Vector3
instance hasYVec3 :: HasY Vector3
instance hasZVec3 :: HasZ Vector3

foreign import showVec2 :: Vector2 -> String
instance showVector2 :: Show Vector2 where
  show = showVec2

foreign import showVec3 :: Vector3 -> String
instance showVector3 :: Show Vector3 where
  show = showVec3

foreign import jsDot            :: forall a. a -> a      -> Number
foreign import jsLength         :: forall a. a -> Number
foreign import jsDist           :: forall a. a -> a      -> Number
foreign import jsClone          :: forall a. a -> a
foreign import jsCross          :: forall a. a -> a      -> a
foreign import jsAdd            :: forall a. a -> a      -> a

foreign import jsAddScaled      :: forall a. a -> a      -> Number -> a
foreign import jsSub            :: forall a. a -> a      -> a
foreign import jsMultiplyScalar :: forall a. a -> Number -> a
foreign import jsNormal         :: forall a. a -> a


class (HasX v, HasY v) <= Vector v where
    dot            :: v -> v -> Number
    length         :: v -> Number
    dist           :: v -> v -> Number
    clone          :: v -> v
    cross          :: v -> v -> v
    add            :: v -> v -> v
    addScaled      :: v -> v -> Number -> v
    sub            :: v -> v -> v
    multiplyScalar :: v -> Number -> v
    normal         :: v -> v

infixr 5 dot as <.>
infixr 6 add as <+>
infixr 6 sub as <->
infixr 7 multiplyScalar as <**>

instance vecVec2 :: Vector Vector2 where
    dot            = jsDot
    length         = jsLength
    dist           = jsDist
    clone          = jsClone
    cross          = jsCross
    add            = jsAdd
    addScaled      = jsAddScaled
    sub            = jsSub
    multiplyScalar = jsMultiplyScalar
    normal         = jsNormal
instance vecVec3 :: Vector Vector3 where
    dot            = jsDot
    length         = jsLength
    dist           = jsDist
    clone          = jsClone
    cross          = jsCross
    add            = jsAdd
    addScaled      = jsAddScaled
    sub            = jsSub
    multiplyScalar = jsMultiplyScalar
    normal         = jsNormal


foreign import applyMatrix :: Matrix4 -> Vector3 -> Vector3

toVec2 :: forall v. Vector v => v -> Vector2
toVec2 v = mkVec2 (vecX v) (vecY v)
