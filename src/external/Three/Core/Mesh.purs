module Three.Core.Mesh where

import Prelude

import Effect (Effect)
import Three.Core.Geometry (class IsBufferGeometry, class IsGeometry, LineGeometry)
import Three.Core.Material (class IsMaterial, LineBasicMaterial)
import Three.Core.Object3D (class IsObject3D)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Mesh :: Type
foreign import mkMesh :: forall geo mat. IsGeometry geo => IsMaterial mat => geo -> mat -> Effect Mesh
instance isObject3DMesh :: IsObject3D Mesh where
    toObject3D = unsafeCoerce

foreign import isMesh :: Mesh -> Boolean

foreign import geometry :: forall geo. IsGeometry geo => Mesh -> geo
foreign import bufferGeometry :: forall geo. IsBufferGeometry geo => Mesh -> geo
foreign import setBufferGeometry :: forall geo. IsBufferGeometry geo => geo -> Mesh -> Effect Unit
foreign import setMaterial :: forall mat. IsMaterial mat => mat -> Mesh -> Effect Unit


foreign import data Line2 :: Type
foreign import mkLine2 :: LineGeometry -> LineBasicMaterial -> Line2
foreign import computeLineDistances :: Line2 -> Effect Unit
instance isObject3DLine2 :: IsObject3D Line2 where
    toObject3D = unsafeCoerce
