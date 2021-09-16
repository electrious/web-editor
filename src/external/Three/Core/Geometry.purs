module Three.Core.Geometry where

import Prelude

import Data.Default (class Default)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Three.Core.Face3 (Face3)
import Three.Core.TypedArray (class IsTypedArray, TypedArray, toTypedArray)
import Three.Math.Shape (Shape)
import Three.Math.Vector (Vector2, Vector3)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class IsGeometry geo where
    toGeometry :: geo -> BufferGeometry

foreign import data BufferGeometry :: Type
instance IsGeometry BufferGeometry where
    toGeometry = identity

foreign import mkBufferGeometry :: Effect BufferGeometry

foreign import jssetAttribute :: String -> BufferAttribute -> BufferGeometry -> Effect Unit
foreign import jsgetAttribute :: String -> BufferGeometry -> BufferAttribute
foreign import jssetIndex :: BufferAttribute -> BufferGeometry -> Effect Unit

setAttribute :: forall geo. IsGeometry geo => String -> BufferAttribute -> geo -> Effect Unit
setAttribute n attr geo = jssetAttribute n attr (toGeometry geo)

getAttribute :: forall geo. IsGeometry geo => String -> geo -> BufferAttribute
getAttribute n geo = jsgetAttribute n (toGeometry geo)

setIndex :: forall geo. IsGeometry geo => BufferAttribute -> geo -> Effect Unit
setIndex attr geo = jssetIndex attr (toGeometry geo)

foreign import jsclone :: BufferGeometry -> Effect BufferGeometry
foreign import jsdispose :: BufferGeometry -> Effect Unit

clone :: forall geo. IsGeometry geo => geo -> Effect geo 
clone geo = unsafeCoerce <$> jsclone (toGeometry geo)

dispose :: forall geo. IsGeometry geo => geo -> Effect Unit
dispose geo = jsdispose (toGeometry geo)

foreign import jscomputeVertexNormals :: BufferGeometry -> Effect Unit

computeVertexNormals :: forall geo. IsGeometry geo => geo -> Effect Unit
computeVertexNormals = jscomputeVertexNormals <<< toGeometry

foreign import data BoxGeometry :: Type
foreign import mkBoxGeometry :: Number -> Number -> Number -> Effect BoxGeometry
instance IsGeometry BoxGeometry where
    toGeometry = unsafeCoerce

foreign import data CircleGeometry :: Type
foreign import mkCircleGeometry :: Number -> Int -> Effect CircleGeometry
instance IsGeometry CircleGeometry where
    toGeometry = unsafeCoerce

foreign import data CylinderGeometry :: Type
foreign import mkCylinderGeometry :: Number -> Number -> Number -> Int -> Boolean -> Effect CylinderGeometry
instance IsGeometry CylinderGeometry where
    toGeometry = unsafeCoerce

foreign import data ConeGeometry :: Type
foreign import mkConeGeometry :: Number -> Number -> Int -> Boolean -> Effect ConeGeometry
instance IsGeometry ConeGeometry where
    toGeometry = unsafeCoerce

foreign import data ShapeGeometry :: Type
foreign import mkShapeGeometry :: Shape -> Effect ShapeGeometry

instance IsGeometry ShapeGeometry where
    toGeometry = unsafeCoerce

foreign import data PlaneGeometry :: Type
foreign import mkPlaneGeometry :: Number -> Number -> Int -> Int -> Effect PlaneGeometry

instance IsGeometry PlaneGeometry where
    toGeometry = unsafeCoerce

-- | ExtrudeGeometry
foreign import data ExtrudeGeometry :: Type
instance IsGeometry ExtrudeGeometry where
    toGeometry = unsafeCoerce

-- | TorusGeometry
foreign import data TorusGeometry :: Type
instance IsGeometry TorusGeometry where
    toGeometry = unsafeCoerce

foreign import mkTorusGeometry :: Number -> Number -> Int -> Int -> Number -> Effect TorusGeometry

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
_curveSegments = _Newtype <<< prop (Proxy :: Proxy "curveSegments")

_steps :: forall t a r. Newtype t { steps :: a | r } => Lens' t a
_steps = _Newtype <<< prop (Proxy :: Proxy "steps")

_depth :: forall t a r. Newtype t { depth :: a | r } => Lens' t a
_depth = _Newtype <<< prop (Proxy :: Proxy "depth")

_bevelEnabled :: forall t a r. Newtype t { bevelEnabled :: a | r } => Lens' t a
_bevelEnabled = _Newtype <<< prop (Proxy :: Proxy "bevelEnabled")

_bevelThickness :: forall t a r. Newtype t { bevelThickness :: a | r } => Lens' t a
_bevelThickness = _Newtype <<< prop (Proxy :: Proxy "bevelThickness")

_bevelSize :: forall t a r. Newtype t { bevelSize :: a | r } => Lens' t a
_bevelSize = _Newtype <<< prop (Proxy :: Proxy "bevelSize")

_bevelOffset :: forall t a r. Newtype t { bevelOffset :: a | r } => Lens' t a
_bevelOffset = _Newtype <<< prop (Proxy :: Proxy "bevelOffset")

_bevelSegments :: forall t a r. Newtype t { bevelSegments :: a | r } => Lens' t a
_bevelSegments = _Newtype <<< prop (Proxy :: Proxy "bevelSegments")

foreign import mkExtrudeGeometry :: Shape -> ExtrudeSettings -> Effect ExtrudeGeometry

foreign import data LineGeometry :: Type
instance IsGeometry LineGeometry where
    toGeometry = unsafeCoerce
foreign import mkLineGeometry :: Array Vector3 -> Effect LineGeometry

foreign import data BufferAttribute :: Type

foreign import jsmkBufferAttribute :: TypedArray -> Int -> Effect BufferAttribute

mkBufferAttribute :: forall arr. IsTypedArray arr => arr -> Int -> Effect BufferAttribute
mkBufferAttribute arr s = jsmkBufferAttribute (toTypedArray arr) s

foreign import isBufferAttribute :: BufferAttribute -> Boolean

foreign import setXYZ :: Int -> Number -> Number -> Number -> BufferAttribute -> Effect Unit

foreign import setNeedsUpdate :: Boolean -> BufferAttribute -> Effect Unit

foreign import count :: BufferAttribute -> Int
foreign import getX :: Int -> BufferAttribute -> Number
foreign import getY :: Int -> BufferAttribute -> Number
foreign import getZ :: Int -> BufferAttribute -> Number


foreign import jsvertices :: BufferGeometry -> Array Vector3
foreign import jsfaces :: BufferGeometry -> Array Face3

vertices :: forall geo. IsGeometry geo => geo -> Array Vector3
vertices = jsvertices <<< toGeometry

faces :: forall geo. IsGeometry geo => geo -> Array Face3
faces = jsfaces <<< toGeometry