module Three.Controls.OrbitControls where

import Prelude

import Effect (Effect)
import Three.Core.Camera (Camera)
import Three.Math.Vector (Vector3)
import Util (ffi, fpi)
import Web.DOM (Element)

foreign import data OrbitControls :: Type
foreign import mkOrbitControls :: forall a. Camera a -> Element -> Effect OrbitControls

update :: OrbitControls -> Effect Unit
update = fpi ["o", ""] "o.update()"

dispose :: OrbitControls -> Effect Unit
dispose = fpi ["o", ""] "o.dispose()"

setAutoRotate :: Boolean -> OrbitControls -> Effect Unit
setAutoRotate = fpi ["r", "o", ""] "o.autoRotate = r"

setAutoRotateSpeed :: Number -> OrbitControls -> Effect Unit
setAutoRotateSpeed = fpi ["s", "o", ""] "o.autoRotateSpeed = s"

isEnabled :: OrbitControls -> Boolean
isEnabled = ffi ["o"] "o.enabled"

setEnabled :: Boolean -> OrbitControls -> Effect Unit
setEnabled = fpi ["e", "o", ""] "o.enabled = e"

enableDamping :: Boolean -> OrbitControls -> Effect Unit
enableDamping = fpi ["e", "o", ""] "o.enableDamping = e"

setDampingFactor :: Number -> OrbitControls -> Effect Unit
setDampingFactor = fpi ["f", "o", ""] "o.dampingFactor = f"

enablePan :: Boolean -> OrbitControls -> Effect Unit
enablePan = fpi ["e", "o", ""] "o.enablePan = e"

setPanSpeed :: Number -> OrbitControls -> Effect Unit
setPanSpeed = fpi ["s", "o", ""] "o.panSpeed = s"

enableRotate :: Boolean -> OrbitControls -> Effect Unit
enableRotate = fpi ["e", "o", ""] "o.enableRotate = e"

setRotateSpeed :: Number -> OrbitControls -> Effect Unit
setRotateSpeed = fpi ["s", "o", ""] "o.rotateSpeed = s"

enableZoom :: Boolean -> OrbitControls -> Effect Unit
enableZoom = fpi ["e", "o", ""] "o.enableZoom = e"

setZoomSpeed :: Number -> OrbitControls -> Effect Unit
setZoomSpeed = fpi ["s", "o", ""] "o.zoomSpeed = s"

setMaxAzimuthAngle :: Number -> OrbitControls -> Effect Unit
setMaxAzimuthAngle = fpi ["a", "o", ""] "o.maxAzimuthAngle = a"

setMinAzimuthAngle :: Number -> OrbitControls -> Effect Unit
setMinAzimuthAngle = fpi ["a", "o", ""] "o.minAzimuthAngle = a"

setMaxDistance :: Number -> OrbitControls -> Effect Unit
setMaxDistance = fpi ["d", "o", ""] "o.maxDistance = d"

setMinDistance :: Number -> OrbitControls -> Effect Unit
setMinDistance = fpi ["d", "o", ""] "o.minDistance = d"

setMaxPolarAngle :: Number -> OrbitControls -> Effect Unit
setMaxPolarAngle = fpi ["a", "o", ""] "o.maxPolarAngle = a"

setMinPolarAngle :: Number -> OrbitControls -> Effect Unit
setMinPolarAngle = fpi ["a", "o", ""] "o.minPolarAngle = a"

setTarget :: Vector3 -> OrbitControls -> Effect Unit
setTarget = fpi ["t", "o", ""] "o.target = t"

