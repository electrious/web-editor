module Three.Core.TypedArray where

import Three.Core.Face3 (Face3)
import Three.Math.Vector (Vector2, Vector3)
import Unsafe.Coerce (unsafeCoerce)

foreign import data TypedArray :: Type

class IsTypedArray a where
    toTypedArray :: a -> TypedArray


foreign import data Uint16Array :: Type
foreign import data Float32Array :: Type

instance IsTypedArray Uint16Array where
    toTypedArray = unsafeCoerce
instance IsTypedArray Float32Array where
    toTypedArray = unsafeCoerce


foreign import vector3Array :: Array Vector3 -> Float32Array
foreign import vector2Array :: Array Vector2 -> Float32Array
foreign import face3Array :: Array Face3 -> Uint16Array
