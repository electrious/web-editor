module Three.Controls.OrbitControls where

import Prelude

import Effect (Effect)
import Three.Core.Camera (class IsCamera)
import Three.Math.Vector (Vector3)
import Web.DOM (Element)

foreign import data OrbitControls :: Type
foreign import mkOrbitControls :: forall c. IsCamera c => c -> Element -> Effect OrbitControls

foreign import update :: OrbitControls -> Effect Unit
foreign import dispose :: OrbitControls -> Effect Unit
foreign import setAutoRotate :: Boolean -> OrbitControls -> Effect Unit
foreign import setAutoRotateSpeed :: Number -> OrbitControls -> Effect Unit
foreign import isEnabled :: OrbitControls -> Boolean
foreign import setEnabled :: Boolean -> OrbitControls -> Effect Unit
foreign import enableDamping :: Boolean -> OrbitControls -> Effect Unit
foreign import setDampingFactor :: Number -> OrbitControls -> Effect Unit
foreign import enablePan :: Boolean -> OrbitControls -> Effect Unit
foreign import setPanSpeed :: Number -> OrbitControls -> Effect Unit
foreign import enableRotate :: Boolean -> OrbitControls -> Effect Unit
foreign import setRotateSpeed :: Number -> OrbitControls -> Effect Unit
foreign import enableZoom :: Boolean -> OrbitControls -> Effect Unit
foreign import setZoomSpeed :: Number -> OrbitControls -> Effect Unit
foreign import setMaxAzimuthAngle :: Number -> OrbitControls -> Effect Unit
foreign import setMinAzimuthAngle :: Number -> OrbitControls -> Effect Unit
foreign import setMaxDistance :: Number -> OrbitControls -> Effect Unit
foreign import setMinDistance :: Number -> OrbitControls -> Effect Unit
foreign import setMaxPolarAngle :: Number -> OrbitControls -> Effect Unit
foreign import setMinPolarAngle :: Number -> OrbitControls -> Effect Unit
foreign import setTarget :: Vector3 -> OrbitControls -> Effect Unit
