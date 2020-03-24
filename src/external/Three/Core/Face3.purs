module Three.Core.Face3 where

import Three.Math.Vector (Vector3)
import Util (ffi)

foreign import data Face3 :: Type

normal :: Face3 -> Vector3
normal = ffi ["face"] "face.normal"