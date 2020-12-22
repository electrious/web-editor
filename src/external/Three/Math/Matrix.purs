module Three.Math.Matrix where

foreign import data Matrix4 :: Type

foreign import invert :: Matrix4 -> Matrix4
