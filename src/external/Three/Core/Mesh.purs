module Three.Core.Mesh where

import Prelude

import Effect (Effect)
import Three.Core.Geometry (class IsBufferGeometry, class IsGeometry, LineGeometry)
import Three.Core.Material (class IsLineMaterial, class IsMaterial)
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


foreign import data Line :: Type
foreign import mkLine :: forall mat. IsLineMaterial mat => LineGeometry -> mat -> Effect Line
foreign import computeLineDistances :: Line -> Effect Unit
instance isObject3DLine :: IsObject3D Line where
    toObject3D = unsafeCoerce
