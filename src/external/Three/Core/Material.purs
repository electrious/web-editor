module Three.Core.Material where

import Prelude

import Effect (Effect)
import Three.Loader.TextureLoader (Texture)
import Util (ffi, fpi)
  
foreign import data Material :: Type
foreign import data MeshBasicMaterial :: Type

foreign import mkMeshBasicMaterial :: Int -> Effect MeshBasicMaterial
foreign import mkMeshBasicMaterialWithTexture :: Texture -> Effect MeshBasicMaterial

class IsMaterial a

instance isMaterialMaterial :: IsMaterial Material
instance isMaterialMeshBasicMaterial :: IsMaterial MeshBasicMaterial

setTransparent :: forall mat. IsMaterial mat => Boolean -> mat -> Effect Unit
setTransparent = fpi ["t", "mat", ""] "mat.transparent = t"

setOpacity :: forall mat. IsMaterial mat => Number -> mat -> Effect Unit
setOpacity = fpi ["o", "mat", ""] "mat.opacity = o"

foreign import data MaterialCreator :: Type

getMaterial :: forall mat. IsMaterial mat => String -> MaterialCreator -> mat
getMaterial = ffi ["mat", "creator"] "creator.materials[mat]"

preload :: MaterialCreator -> Effect Unit
preload = fpi ["creator", ""] "creator.preload()"
