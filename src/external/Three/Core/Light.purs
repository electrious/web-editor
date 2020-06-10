module Three.Core.Light where

import Effect (Effect)
import Three.Core.Object3D (class IsObject3D)

foreign import data Light :: Type
foreign import data AmbientLight :: Type

foreign import mkAmbientLight :: Int -> Effect AmbientLight

foreign import data DirectionalLight :: Type
foreign import mkDirectionalLight :: Int -> Number -> Effect DirectionalLight

instance isObject3DLight :: IsObject3D Light
instance isObject3DAmbientLight :: IsObject3D AmbientLight
instance isObject3DDirectionalLight :: IsObject3D DirectionalLight

class IsLight l

instance isLightAmbientLight :: IsLight AmbientLight
instance isLightDirectionalLight :: IsLight DirectionalLight
