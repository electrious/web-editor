module Three.Core.Geometry where

import Prelude

import Effect (Effect)
import Three.Math.Vector (Vector2)
import Util (ffi, fpi)

foreign import data JSGeometry :: Type -> Type
foreign import data JSBufferGeometry :: Type -> Type
foreign import data BufferAttribute :: Type

type Geometry a = JSGeometry a
type BufferGeometry a = Geometry (JSBufferGeometry a)

clone :: forall a. Geometry a -> Effect (Geometry a)
clone = ffi ["geo", ""] "geo.clone()"

foreign import data JSCircleGeometry :: Type -> Type
type CircleGeometry a = Geometry (JSCircleGeometry a)

mkCircleGeometry :: forall a. Number -> Int -> Effect (CircleGeometry a)
mkCircleGeometry = ffi ["radius", "segs", ""] "new THREE.CircleGeometry(radius, segs)"


foreign import data JSShapeGeometry :: Type -> Type
type ShapeGeometry a = Geometry (JSShapeGeometry a)

foreign import data Shape :: Type

mkShape :: Array Vector2 -> Effect Shape
mkShape = ffi ["ps", ""] "new THREE.Shape(ps)"

mkShapeGeometry :: forall a. Shape -> Effect (ShapeGeometry a)
mkShapeGeometry = ffi ["shp", ""] "new THREE.ShapeGeometry(shp)"

isBufferGeometry :: forall a. Geometry a -> Boolean
isBufferGeometry = ffi ["geo"] "geo instanceof THREE.BufferGeometry"

getAttribute :: forall a. String -> BufferGeometry a -> BufferAttribute
getAttribute = ffi ["name", "geo"] "geo.getAttribute(name)"


isBufferAttribute :: BufferAttribute -> Boolean
isBufferAttribute = ffi ["attr"] "attr instanceof THREE.BufferAttribute"

setXYZ :: Int -> Number -> Number -> Number -> BufferAttribute -> Effect Unit
setXYZ = fpi ["idx", "x", "y", "z", "attr", ""] "attr.setXYZ(idx, x, y, z)"

setNeedsUpdate :: Boolean -> BufferAttribute -> Effect Unit
setNeedsUpdate = fpi ["u", "attr", ""] "attr.needsUpdate = u"

count :: BufferAttribute -> Int
count = ffi ["attr"] "attr.count"

getX :: Int -> BufferAttribute -> Number
getX = ffi ["idx", "attr"] "attr.getX(idx)"

getY :: Int -> BufferAttribute -> Number
getY = ffi ["idx", "attr"] "attr.getX(idx)"

getZ :: Int -> BufferAttribute -> Number
getZ = ffi ["idx", "attr"] "attr.getX(idx)"
