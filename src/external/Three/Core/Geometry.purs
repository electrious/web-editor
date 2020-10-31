module Three.Core.Geometry where

import Prelude

import Effect (Effect)
import Three.Core.Face3 (Face3)
import Three.Math.Color (Color)
import Three.Math.Vector (Vector2, Vector3)

class IsGeometry geo
class IsGeometry geo <= IsBufferGeometry geo

foreign import clone :: forall geo. IsGeometry geo => geo -> Effect geo

foreign import data Geometry :: Type
instance isGeometryGeometry :: IsGeometry Geometry

foreign import mkGeometry :: Effect Geometry

foreign import vertices :: forall geo. IsGeometry geo => geo -> Array Vector3
foreign import setVertices :: forall geo. IsGeometry geo => Array Vector3 -> geo -> Effect Unit
foreign import setVerticesNeedUpdate :: forall geo. IsGeometry geo => Boolean -> geo -> Effect Unit

foreign import faces :: forall geo. IsGeometry geo => geo -> Array Face3
foreign import setFaces :: forall geo. IsGeometry geo => Array Face3 -> geo -> Effect Unit
foreign import setElementsNeedUpdate :: forall geo. IsGeometry geo => Boolean -> geo -> Effect Unit

foreign import setUVs :: forall geo. IsGeometry geo => Array (Array Vector2) -> geo -> Effect Unit
foreign import setUVsNeedUpdate :: forall geo. IsGeometry geo => Boolean -> geo -> Effect Unit

foreign import data BoxGeometry :: Type
foreign import mkBoxGeometry :: Number -> Number -> Number -> Effect BoxGeometry
instance isGeometryBoxGeometry :: IsGeometry BoxGeometry

foreign import data CircleGeometry :: Type
foreign import mkCircleGeometry :: Number -> Int -> Effect CircleGeometry
instance isGeometryCircleGeometry :: IsGeometry CircleGeometry

foreign import data CylinderGeometry :: Type
foreign import mkCylinderGeometry :: Number -> Number -> Effect CylinderGeometry
instance isGeometryCylinderGeometry :: IsGeometry CylinderGeometry

foreign import data ShapeGeometry :: Type
foreign import data Shape :: Type
foreign import mkShape :: Array Vector2 -> Effect Shape
foreign import mkShapeGeometry :: Shape -> Effect ShapeGeometry

instance isGeometryShapeGeometry :: IsGeometry ShapeGeometry

foreign import data PlaneGeometry :: Type
foreign import mkPlaneGeometry :: Number -> Number -> Int -> Int -> Effect PlaneGeometry

instance isGeometryPlaneGeometry :: IsGeometry PlaneGeometry

foreign import data LineGeometry :: Type
foreign import mkLineGeometry :: Effect LineGeometry
foreign import setLinePositions :: Array Vector2 -> LineGeometry -> Effect Unit
foreign import setLineColors :: Array Color -> LineGeometry -> Effect Unit

foreign import data BufferGeometry :: Type
foreign import data BufferAttribute :: Type

instance isGeometryBufferGeometry :: IsGeometry BufferGeometry
instance isBufferGeometryBufferGeometry :: IsBufferGeometry BufferGeometry

foreign import getAttribute :: forall geo. IsBufferGeometry geo => String -> geo -> BufferAttribute
foreign import isBufferAttribute :: BufferAttribute -> Boolean

foreign import setXYZ :: Int -> Number -> Number -> Number -> BufferAttribute -> Effect Unit

foreign import setNeedsUpdate :: Boolean -> BufferAttribute -> Effect Unit

foreign import count :: BufferAttribute -> Int
foreign import getX :: Int -> BufferAttribute -> Number
foreign import getY :: Int -> BufferAttribute -> Number
foreign import getZ :: Int -> BufferAttribute -> Number
