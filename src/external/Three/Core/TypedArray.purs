module Three.Core.TypedArray where

import Three.Core.Face3 (Face3)
import Three.Math.Vector (Vector2, Vector3)

class TypedArray :: forall k. k -> Constraint
class TypedArray a

foreign import data Uint16Array :: Type
foreign import data Float32Array :: Type

instance typedArrayUint16Array :: TypedArray Uint16Array
instance typedArrayFloat32Array :: TypedArray Float32Array


foreign import vector3Array :: Array Vector3 -> Float32Array
foreign import vector2Array :: Array Vector2 -> Float32Array
foreign import face3Array :: Array Face3 -> Uint16Array
