module SmartHouse.Algorithm.VertInfo where

import Prelude

import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Editor.Common.Lenses (_slope)
import Math (abs)
import Math.Angle (Angle, acos, degreeVal, tan)
import Math.LineSeg (direction)
import SmartHouse.Algorithm.EdgeInfo (EdgeInfo, _line)
import SmartHouse.Algorithm.Ray (Ray, ray)
import Three.Math.Vector (class Vector, Vector3, normal, vecX, vecY, (<**>), (<+>), (<.>))
import Type.Proxy (Proxy(..))


-- | data value used to pass vertice and edge slope info to the skeleton algorithm.
newtype VertWithSlope = VertWithSlope {
    position :: Vector3,
    slope    :: Angle
}

derive instance Newtype VertWithSlope _

vertWithSlope :: Vector3 -> Angle -> VertWithSlope
vertWithSlope v slope = VertWithSlope { position : v, slope : slope }

-- intermediete data structure for constructing initial vertices and edges
newtype VertInfo = VertInfo {
    position  :: Vector3,
    height    :: Number,
    isReflex  :: Boolean,
    bisector  :: Ray,
    usable    :: Boolean,

    leftEdge  :: EdgeInfo,
    rightEdge :: EdgeInfo
    }

derive instance newtypeVertInfo :: Newtype VertInfo _

_isReflex :: forall t a r. Newtype t { isReflex :: a | r } => Lens' t a
_isReflex = _Newtype <<< prop (Proxy :: Proxy "isReflex")

_bisector :: forall t a r. Newtype t { bisector :: a | r } => Lens' t a
_bisector = _Newtype <<< prop (Proxy :: Proxy "bisector")

_usable :: forall t a r. Newtype t { usable :: a | r } => Lens' t a
_usable = _Newtype <<< prop (Proxy :: Proxy "usable")

_cross :: forall v. Vector v => v -> v -> Number
_cross v1 v2 = vecX v1 * vecY v2 - vecX v2 * vecY v1


-- | calculate the bisector vector of two edges with their slopes
calcDir :: Vector3 -> Angle -> Vector3 -> Angle -> Boolean -> Tuple Vector3 Boolean
calcDir leftV leftSlope rightV rightSlope isReflex =
    let lt = tan leftSlope
        rt = tan rightSlope
    
        dir = (leftV <**> lt <+> rightV <**> rt) <**> (if isReflex then -1.0 else 1.0)
        a = acos $ leftV <.> rightV
        -- the bisector is usable only if the two edges are not parallel
        -- based on the angle between the edges are not smaller than 2 degrees
        usable = abs (degreeVal a - 90.0) < 2.0
    in Tuple dir usable

vertInfoFrom :: Vector3 -> Number -> EdgeInfo -> EdgeInfo-> Maybe Vector3 -> Maybe Vector3 -> VertInfo
vertInfoFrom p h leftEdge rightEdge vecL vecR =
    let leftVec  = direction (leftEdge ^. _line) <**> (-1.0)
        rightVec = direction $ rightEdge ^. _line
        lv       = fromMaybe leftVec $ normal <$> vecL
        rv       = fromMaybe rightVec $ normal <$> vecR
        isReflex = _cross lv rv < 0.0

        lSlope   = leftEdge  ^. _slope
        rSlope   = rightEdge ^. _slope

        Tuple dir usable = calcDir leftVec lSlope rightVec rSlope isReflex
    in VertInfo {
        position  : p,
        height    : h,
        isReflex  : isReflex,
        bisector  : ray p dir,
        usable    : usable,

        leftEdge  : leftEdge,
        rightEdge : rightEdge
        }
