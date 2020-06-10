module Three.Core.Scene where

import Prelude

import Effect (Effect)
import Three.Core.Object3D (class IsObject3D)
import Util (fpi)

foreign import data Scene :: Type
foreign import mkScene :: Effect Scene

instance isObject3DScene :: IsObject3D Scene

disposeScene :: Scene -> Effect Unit
disposeScene = fpi ["s", ""] "s.dispose()"
