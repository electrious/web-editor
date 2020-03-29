module Three.Core.Mesh where

import Prelude

import Effect (Effect)
import Three.Core.Geometry (class IsGeometry, BufferGeometry, Geometry)
import Three.Core.Material (Material)
import Three.Core.Object3D (Object3D)
import Util (ffi, fpi)

foreign import data JSMesh :: Type -> Type

type Mesh a = Object3D (JSMesh a)

mkMesh :: forall a b geo. IsGeometry geo => geo -> Material a -> Effect (Mesh b)
mkMesh = ffi ["geo", "mat", ""] "new THREE.Mesh(geo, mat)"

isMesh :: forall a. Mesh a -> Boolean
isMesh = ffi ["m"] "m instanceof THREE.Mesh"

geometry :: forall a. Mesh a -> Geometry
geometry = ffi ["mesh"] "mesh.geometry"

bufferGeometry :: forall a. Mesh a -> BufferGeometry
bufferGeometry = ffi ["mesh"] "mesh.geometry"

setBufferGeometry :: forall a. BufferGeometry -> Mesh a -> Effect Unit
setBufferGeometry = fpi ["geo", "mesh", ""] "mesh.geometry = geo"
