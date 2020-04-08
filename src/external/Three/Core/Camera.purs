module Three.Core.Camera where

import Prelude

import Effect (Effect)
import Three.Core.Object3D (Object3D)
import Util (fpi)

foreign import data JSCamera :: Type -> Type

type Camera a = Object3D (JSCamera a)

foreign import data JSPerspectiveCamera :: Type -> Type
type PerspectiveCamera a = Camera (JSPerspectiveCamera a)

foreign import mkPerspectiveCamera :: forall a. Number -> Number -> Number -> Number -> Effect (PerspectiveCamera a)

setAspect :: forall a. Number -> PerspectiveCamera a -> Effect Unit
setAspect = fpi ["aspect", "c", ""] "c.aspect = aspect"

updateProjectionMatrix :: forall a. PerspectiveCamera a -> Effect Unit
updateProjectionMatrix = fpi ["c", ""] "c.updateProjectionMatrix()"
