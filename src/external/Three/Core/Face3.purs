module Three.Core.Face3 where

import Effect (Effect)
import Three.Math.Vector (Vector3)

foreign import data Face3 :: Type

foreign import mkFace3 :: Int -> Int -> Int -> Effect Face3

foreign import indexA :: Face3 -> Int
foreign import indexB :: Face3 -> Int
foreign import indexC :: Face3 -> Int
foreign import normal :: Face3 -> Vector3