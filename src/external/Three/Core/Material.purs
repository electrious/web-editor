module Three.Core.Material where

import Prelude

import Effect (Effect)
import Three.Loader.TextureLoader (Texture)
  
foreign import data Material :: Type
foreign import data MeshBasicMaterial :: Type
foreign import data LineBasicMaterial :: Type

foreign import mkMeshBasicMaterial :: Int -> Effect MeshBasicMaterial
foreign import mkMeshBasicMaterialWithTexture :: Texture -> Effect MeshBasicMaterial

-- create LineBasicMaterial with color and line width
foreign import mkLineBasicMaterial :: Int -> Number -> Effect LineBasicMaterial

class IsMaterial a

instance isMaterialMaterial :: IsMaterial Material
instance isMaterialMeshBasicMaterial :: IsMaterial MeshBasicMaterial

foreign import setTransparent :: forall mat. IsMaterial mat => Boolean -> mat -> Effect Unit

foreign import setOpacity :: forall mat. IsMaterial mat => Number -> mat -> Effect Unit

foreign import data Side :: Type
foreign import frontSide :: Side
foreign import backSide :: Side
foreign import doubleSide :: Side

foreign import setSide :: forall mat. IsMaterial mat => Side -> mat -> Effect Unit

foreign import data MaterialCreator :: Type

foreign import getMaterial :: forall mat. IsMaterial mat => String -> MaterialCreator -> mat
foreign import preload :: MaterialCreator -> Effect Unit
