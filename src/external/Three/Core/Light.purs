module Three.Core.Light where

import Effect (Effect)
import Three.Core.Object3D (Object3D)
import Util (ffi)

foreign import data JSLight :: Type -> Type

type Light a = Object3D (JSLight a)

foreign import data JSAmbientLight :: Type -> Type
type AmbientLight a = Light (JSAmbientLight a)

mkAmbientLight :: forall a. Int -> Effect (AmbientLight a)
mkAmbientLight = ffi ["c", ""] "new THREE.AmbientLight(c)"

foreign import data JSDirectionalLight :: Type -> Type
type DirectionalLight a = Light (JSDirectionalLight a)

mkDirectionalLight :: forall a. Int -> Number -> Effect (DirectionalLight a)
mkDirectionalLight = ffi ["color", "intensity", ""] "new THREE.DirectionalLight(color, intensity)"
