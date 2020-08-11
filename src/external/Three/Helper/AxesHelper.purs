module Three.Helper.AxesHelper where

import Effect (Effect)
import Three.Core.Object3D (class IsObject3D)
import Unsafe.Coerce (unsafeCoerce)

foreign import data AxesHelper :: Type
foreign import mkAxesHelper :: Effect AxesHelper

instance isObject3DAxesHelper :: IsObject3D AxesHelper where
    toObject3D = unsafeCoerce
