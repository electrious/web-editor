module Three.Core.Camera where

import Prelude

import Effect (Effect)
import Three.Core.Object3D (class IsObject3D)
import Unsafe.Coerce (unsafeCoerce)
import Util (fpi)

foreign import data Camera :: Type
foreign import data PerspectiveCamera :: Type

foreign import mkPerspectiveCamera :: Number -> Number -> Number -> Number -> Effect PerspectiveCamera

instance isObject3DCamera :: IsObject3D Camera where
    toObject3D = unsafeCoerce
instance isObject3DPerspectiveCamera :: IsObject3D PerspectiveCamera where
    toObject3D = unsafeCoerce

class IsObject3D a <= IsCamera a

instance isCameraPerspectiveCamera :: IsCamera PerspectiveCamera

setAspect :: Number -> PerspectiveCamera -> Effect Unit
setAspect = fpi ["aspect", "c", ""] "c.aspect = aspect"

updateProjectionMatrix :: PerspectiveCamera -> Effect Unit
updateProjectionMatrix = fpi ["c", ""] "c.updateProjectionMatrix()"
