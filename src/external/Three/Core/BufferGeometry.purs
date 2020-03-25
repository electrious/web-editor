module Three.Core.BufferGeometry where

import Prelude

import Effect (Effect)
import Util (ffi, fpi)

foreign import data BufferGeometry :: Type
foreign import data BufferAttribute :: Type

clone :: BufferGeometry -> Effect BufferGeometry
clone = ffi ["geo", ""] "geo.clone()"

getAttribute :: String -> BufferGeometry -> BufferAttribute
getAttribute = ffi ["name", "geo"] "geo.getAttribute(name)"


isBufferAttribute :: BufferAttribute -> Boolean
isBufferAttribute = ffi ["attr"] "attr instanceof THREE.BufferAttribute"

setXYZ :: Int -> Number -> Number -> Number -> BufferAttribute -> Effect Unit
setXYZ = fpi ["idx", "x", "y", "z", "attr", ""] "attr.setXYZ(idx, x, y, z)"

setNeedsUpdate :: Boolean -> BufferAttribute -> Effect Unit
setNeedsUpdate = fpi ["u", "attr", ""] "attr.needsUpdate = u"
