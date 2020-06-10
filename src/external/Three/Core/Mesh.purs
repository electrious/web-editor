module Three.Core.Mesh where

import Prelude

import Effect (Effect)
import Three.Core.Geometry (class IsBufferGeometry, class IsGeometry)
import Three.Core.Material (class IsMaterial)
import Three.Core.Object3D (class IsObject3D)
import Util (ffi, fpi)

foreign import data Mesh :: Type
foreign import mkMesh :: forall geo mat. IsGeometry geo => IsMaterial mat => geo -> mat -> Effect Mesh
instance isObject3DMesh :: IsObject3D Mesh

foreign import isMesh :: Mesh -> Boolean

geometry :: forall geo. IsGeometry geo => Mesh -> geo
geometry = ffi ["mesh"] "mesh.geometry"

bufferGeometry :: forall geo. IsBufferGeometry geo => Mesh -> geo
bufferGeometry = ffi ["mesh"] "mesh.geometry"

setBufferGeometry :: forall geo. IsBufferGeometry geo => geo -> Mesh -> Effect Unit
setBufferGeometry = fpi ["geo", "mesh", ""] "mesh.geometry = geo"

setMaterial :: forall mat. IsMaterial mat => mat -> Mesh -> Effect Unit
setMaterial = fpi ["mat", "mesh", ""] "mesh.material = mat"
