module Three.Core.Geometry where

import Prelude

import Data.Default (class Default)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Three.Core.Face3 (Face3)
import Three.Core.TypedArray (class TypedArray)
import Three.Math.Vector (Vector2, Vector3)

class IsGeometry geo

foreign import data BufferGeometry :: Type
instance isGeometryBufferGeometry :: IsGeometry BufferGeometry

foreign import mkBufferGeometry :: Effect BufferGeometry

foreign import setAttribute :: forall geo. IsGeometry geo => String -> BufferAttribute -> geo -> Effect Unit
foreign import getAttribute :: forall geo. IsGeometry geo => String -> geo -> BufferAttribute
foreign import setIndex :: forall geo. IsGeometry geo => BufferAttribute -> geo -> Effect Unit


foreign import clone :: forall geo. IsGeometry geo => geo -> Effect geo

foreign import computeVertexNormals :: forall geo. IsGeometry geo => geo -> Effect Unit

foreign import data BoxGeometry :: Type
foreign import mkBoxGeometry :: Number -> Number -> Number -> Effect BoxGeometry
instance isGeometryBoxGeometry :: IsGeometry BoxGeometry

foreign import data CircleGeometry :: Type
foreign import mkCircleGeometry :: Number -> Int -> Effect CircleGeometry
instance isGeometryCircleGeometry :: IsGeometry CircleGeometry

foreign import data CylinderGeometry :: Type
foreign import mkCylinderGeometry :: Number -> Number -> Number -> Int -> Effect CylinderGeometry
instance isGeometryCylinderGeometry :: IsGeometry CylinderGeometry

foreign import data ShapeGeometry :: Type
foreign import data Shape :: Type
foreign import mkShape :: Array Vector2 -> Effect Shape
foreign import mkShapeGeometry :: Shape -> Effect ShapeGeometry

instance isGeometryShapeGeometry :: IsGeometry ShapeGeometry

foreign import data PlaneGeometry :: Type
foreign import mkPlaneGeometry :: Number -> Number -> Int -> Int -> Effect PlaneGeometry

instance isGeometryPlaneGeometry :: IsGeometry PlaneGeometry

-- | ExtrudeGeometry
foreign import data ExtrudeGeometry :: Type
instance isGeometryExtrudeGeometry :: IsGeometry ExtrudeGeometry

-- | ExtrudeSettings
newtype ExtrudeSettings = ExtrudeSettings {
    curveSegments  :: Int,
    steps          :: Int,
    depth          :: Number,
    bevelEnabled   :: Boolean,
    bevelThickness :: Number,
    bevelSize      :: Number,
    bevelOffset    :: Number,
    bevelSegments  :: Int
    }

derive instance newtypeExtrudeSettings :: Newtype ExtrudeSettings _

instance defaultExtrudeSettings :: Default ExtrudeSettings where
    def = ExtrudeSettings {
        curveSegments  : 12,
        steps          : 1,
        depth          : 100.0,
        bevelEnabled   : true,
        bevelThickness : 6.0,
        bevelSize      : 2.0,
        bevelOffset    : 0.0,
        bevelSegments  : 3
        }

_curveSegments :: forall t a r. Newtype t { curveSegments :: a | r } => Lens' t a
_curveSegments = _Newtype <<< prop (SProxy :: SProxy "curveSegments")

_steps :: forall t a r. Newtype t { steps :: a | r } => Lens' t a
_steps = _Newtype <<< prop (SProxy :: SProxy "steps")

_depth :: forall t a r. Newtype t { depth :: a | r } => Lens' t a
_depth = _Newtype <<< prop (SProxy :: SProxy "depth")

_bevelEnabled :: forall t a r. Newtype t { bevelEnabled :: a | r } => Lens' t a
_bevelEnabled = _Newtype <<< prop (SProxy :: SProxy "bevelEnabled")

_bevelThickness :: forall t a r. Newtype t { bevelThickness :: a | r } => Lens' t a
_bevelThickness = _Newtype <<< prop (SProxy :: SProxy "bevelThickness")

_bevelSize :: forall t a r. Newtype t { bevelSize :: a | r } => Lens' t a
_bevelSize = _Newtype <<< prop (SProxy :: SProxy "bevelSize")

_bevelOffset :: forall t a r. Newtype t { bevelOffset :: a | r } => Lens' t a
_bevelOffset = _Newtype <<< prop (SProxy :: SProxy "bevelOffset")

_bevelSegments :: forall t a r. Newtype t { bevelSegments :: a | r } => Lens' t a
_bevelSegments = _Newtype <<< prop (SProxy :: SProxy "bevelSegments")

foreign import mkExtrudeGeometry :: Shape -> ExtrudeSettings -> Effect ExtrudeGeometry

foreign import data LineGeometry :: Type
foreign import mkLineGeometry :: Array Vector3 -> Effect LineGeometry

foreign import data BufferAttribute :: Type

foreign import mkBufferAttribute :: forall a. TypedArray a => a -> Int -> Effect BufferAttribute

foreign import isBufferAttribute :: BufferAttribute -> Boolean

foreign import setXYZ :: Int -> Number -> Number -> Number -> BufferAttribute -> Effect Unit

foreign import setNeedsUpdate :: Boolean -> BufferAttribute -> Effect Unit

foreign import count :: BufferAttribute -> Int
foreign import getX :: Int -> BufferAttribute -> Number
foreign import getY :: Int -> BufferAttribute -> Number
foreign import getZ :: Int -> BufferAttribute -> Number


foreign import vertices :: forall geo. IsGeometry geo => geo -> Array Vector3
foreign import faces :: forall geo. IsGeometry geo => geo -> Array Face3
