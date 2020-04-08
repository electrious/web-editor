module Three.Core.Light where

import Effect (Effect)
import Three.Core.Object3D (Object3D)

foreign import data JSLight :: Type -> Type

type Light a = Object3D (JSLight a)

foreign import data JSAmbientLight :: Type -> Type
type AmbientLight a = Light (JSAmbientLight a)

foreign import mkAmbientLight :: forall a. Int -> Effect (AmbientLight a)

foreign import data JSDirectionalLight :: Type -> Type
type DirectionalLight a = Light (JSDirectionalLight a)

foreign import mkDirectionalLight :: forall a. Int -> Number -> Effect (DirectionalLight a)
