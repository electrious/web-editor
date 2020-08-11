module Three.Core.Light where

import Effect (Effect)
import Three.Core.Object3D (class IsObject3D)
import Unsafe.Coerce (unsafeCoerce)

foreign import data AmbientLight :: Type

foreign import mkAmbientLight :: Int -> Effect AmbientLight

foreign import data DirectionalLight :: Type
foreign import mkDirectionalLight :: Int -> Number -> Effect DirectionalLight

instance isObject3DAmbientLight :: IsObject3D AmbientLight where
    toObject3D = unsafeCoerce
instance isObject3DDirectionalLight :: IsObject3D DirectionalLight where
    toObject3D = unsafeCoerce

class IsObject3D l <= IsLight l

instance isLightAmbientLight :: IsLight AmbientLight
instance isLightDirectionalLight :: IsLight DirectionalLight
