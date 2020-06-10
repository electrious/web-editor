module Three.Core.Geometry where

import Prelude

import Effect (Effect)
import Three.Math.Vector (Vector2)
import Util (ffi, fpi)

class IsGeometry geo
class IsGeometry geo <= IsBufferGeometry geo

clone :: forall geo. IsGeometry geo => geo -> Effect geo
clone = ffi ["geo", ""] "geo.clone()"

foreign import data Geometry :: Type
instance isGeometryGeometry :: IsGeometry Geometry

foreign import data BoxGeometry :: Type
foreign import mkBoxGeometry :: Number -> Number -> Number -> Effect BoxGeometry
instance isGeometryBoxGeometry :: IsGeometry BoxGeometry

foreign import data CircleGeometry :: Type
foreign import mkCircleGeometry :: Number -> Int -> Effect CircleGeometry
instance isGeometryCircleGeometry :: IsGeometry CircleGeometry

foreign import data CylinderGeometry :: Type
foreign import mkCylinderGeometry :: Number -> Number -> Effect CylinderGeometry
instance isGeometryCylinderGeometry :: IsGeometry CylinderGeometry

foreign import data ShapeGeometry :: Type
foreign import data Shape :: Type
foreign import mkShape :: Array Vector2 -> Effect Shape
foreign import mkShapeGeometry :: Shape -> Effect ShapeGeometry

instance isGeometryShapeGeometry :: IsGeometry ShapeGeometry

foreign import data BufferGeometry :: Type
foreign import data BufferAttribute :: Type

instance isGeometryBufferGeometry :: IsGeometry BufferGeometry
instance isBufferGeometryBufferGeometry :: IsBufferGeometry BufferGeometry

foreign import isBufferGeometry :: forall geo. IsGeometry geo => geo -> Boolean

getAttribute :: forall geo. IsBufferGeometry geo => String -> geo -> BufferAttribute
getAttribute = ffi ["name", "geo"] "geo.getAttribute(name)"

foreign import isBufferAttribute :: BufferAttribute -> Boolean

setXYZ :: Int -> Number -> Number -> Number -> BufferAttribute -> Effect Unit
setXYZ = fpi ["idx", "x", "y", "z", "attr", ""] "attr.setXYZ(idx, x, y, z)"

setNeedsUpdate :: Boolean -> BufferAttribute -> Effect Unit
setNeedsUpdate = fpi ["u", "attr", ""] "attr.needsUpdate = u"

count :: BufferAttribute -> Int
count = ffi ["attr"] "attr.count"

getX :: Int -> BufferAttribute -> Number
getX = ffi ["idx", "attr"] "attr.getX(idx)"

getY :: Int -> BufferAttribute -> Number
getY = ffi ["idx", "attr"] "attr.getY(idx)"

getZ :: Int -> BufferAttribute -> Number
getZ = ffi ["idx", "attr"] "attr.getZ(idx)"
