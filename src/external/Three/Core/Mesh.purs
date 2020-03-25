module Three.Core.Mesh where

import Prelude

import Effect (Effect)
import Three.Core.BufferGeometry (BufferGeometry)
import Three.Core.Object3D (Object3D)
import Util (ffi, fpi)

foreign import data JSMesh :: Type -> Type

type Mesh a = Object3D (JSMesh a)

bufferGeometry :: forall a. Mesh a -> BufferGeometry
bufferGeometry = ffi ["mesh"] "mesh.geometry"

setBufferGeometry :: forall a. BufferGeometry -> Mesh a -> Effect Unit
setBufferGeometry = fpi ["geo", "mesh", ""] "mesh.geometry = geo"
