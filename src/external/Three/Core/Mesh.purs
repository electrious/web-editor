module Three.Core.Mesh where

import Prelude

import Effect (Effect)
import Three.Core.Geometry (BufferGeometry, Geometry)
import Three.Core.Material (Material)
import Three.Core.Object3D (Object3D)
import Util (ffi, fpi)

foreign import data JSMesh :: Type -> Type

type Mesh a = Object3D (JSMesh a)

mkMesh :: forall a geo mat. Geometry geo -> Material mat -> Effect (Mesh a)
mkMesh = ffi ["geo", "mat", ""] "new THREE.Mesh(geo, mat)"

isMesh :: forall a. Mesh a -> Boolean
isMesh = ffi ["m"] "m instanceof THREE.Mesh"

geometry :: forall a geo. Mesh a -> Geometry geo
geometry = ffi ["mesh"] "mesh.geometry"

bufferGeometry :: forall a geo. Mesh a -> BufferGeometry geo
bufferGeometry = ffi ["mesh"] "mesh.geometry"

setBufferGeometry :: forall a geo. BufferGeometry geo -> Mesh a -> Effect Unit
setBufferGeometry = fpi ["geo", "mesh", ""] "mesh.geometry = geo"

setMaterial :: forall a mat. Material mat -> Mesh a -> Effect Unit
setMaterial = fpi ["mat", "mesh", ""] "mesh.material = mat"
