module Three.Core.Material where

import Prelude

import Effect (Effect)
import Util (ffi, fpi)
  
foreign import data JSMaterial :: Type -> Type

type Material a = JSMaterial a

foreign import data JSMeshBasicMaterial :: Type -> Type

type MeshBasicMaterial a = Material (JSMeshBasicMaterial a)

foreign import mkMeshBasicMaterial :: forall a. Int -> Effect (MeshBasicMaterial a)

setTransparent :: forall a. Boolean -> Material a -> Effect Unit
setTransparent = fpi ["t", "mat", ""] "mat.transparent = t"

setOpacity :: forall a. Number -> Material a -> Effect Unit
setOpacity = fpi ["o", "mat", ""] "mat.opacity = o"

foreign import data MaterialCreator :: Type

getMaterial :: forall a. String -> MaterialCreator -> Material a
getMaterial = ffi ["mat", "creator"] "creator.materials[mat]"

preload :: MaterialCreator -> Effect Unit
preload = fpi ["creator", ""] "creator.preload()"
