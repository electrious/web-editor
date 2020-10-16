module Three.Core.Face3 where

import Effect (Effect)
import Three.Math.Vector (Vector3)
import Util (ffi)

foreign import data Face3 :: Type

foreign import mkFace3 :: Int -> Int -> Int -> Effect Face3

indexA :: Face3 -> Int
indexA = ffi ["face"] "face.a"

indexB :: Face3 -> Int
indexB = ffi ["face"] "face.b"

indexC :: Face3 -> Int
indexC = ffi ["face"] "face.c"

normal :: Face3 -> Vector3
normal = ffi ["face"] "face.normal"