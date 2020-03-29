module Three.Core.Geometry where

import Prelude

import Effect (Effect)
import Util (ffi, fpi)

foreign import data Geometry :: Type
foreign import data BufferGeometry :: Type
foreign import data BufferAttribute :: Type

class IsGeometry a where
    clone :: a -> Effect a

instance isGeo :: IsGeometry Geometry where
    clone = jsClone

instance isBuffGeo :: IsGeometry BufferGeometry where
    clone = jsClone

jsClone :: forall a. a -> Effect a
jsClone = ffi ["geo", ""] "geo.clone()"

isBufferGeometry :: forall a. IsGeometry a => a -> Boolean
isBufferGeometry = ffi ["geo"] "geo instanceof THREE.BufferGeometry"

getAttribute :: String -> BufferGeometry -> BufferAttribute
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
