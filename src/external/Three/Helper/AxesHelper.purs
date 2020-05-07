module Three.Helper.AxesHelper where

import Effect (Effect)
import Three.Core.Object3D (Object3D)

foreign import data JSAxesHelper :: Type -> Type

type AxesHelper a = Object3D (JSAxesHelper a)

foreign import mkAxesHelper :: forall a. Effect (AxesHelper a)
