module Three.Core.Camera where

import Three.Core.Object3D (Object3D)

foreign import data JSCamera :: Type -> Type

type Camera a = Object3D (JSCamera a)
