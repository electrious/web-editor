module Three.Core.Material where

import Prelude

import Effect (Effect)
import Util (ffi, fpi)
  
foreign import data JSMaterial :: Type -> Type

type Material a = JSMaterial a

setTransparent :: forall a. Boolean -> Material a -> Effect Unit
setTransparent = fpi ["t", "mat", ""] "mat.transparent = t"

foreign import data MaterialCreator :: Type

getMaterial :: forall a. String -> MaterialCreator -> Material a
getMaterial = ffi ["mat", "creator"] "creator.materials[mat]"

preload :: MaterialCreator -> Effect Unit
preload = fpi ["creator", ""] "creator.preload()"
