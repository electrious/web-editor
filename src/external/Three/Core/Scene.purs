module Three.Core.Scene where

import Prelude

import Effect (Effect)
import Three.Core.Object3D (Object3D)
import Util (ffi, fpi)

foreign import data JSScene :: Type -> Type

type Scene a = Object3D (JSScene a)

mkScene :: forall a. Effect (Scene a)
mkScene = ffi [""] "new THREE.Scene()"

disposeScene :: forall a. Scene a -> Effect Unit
disposeScene = fpi ["s", ""] "s.dispose()"
