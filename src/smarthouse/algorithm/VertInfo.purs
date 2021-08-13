module SmartHouse.Algorithm.VertInfo where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', set, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Data.UUIDWrapper (UUID, genUUID)
import Editor.Common.Lenses (_id, _slope)
import Effect (Effect)
import Math (abs)
import Math.Angle (Angle, acos, degreeVal, tan)
import Math.LineSeg (direction)
import Model.UUID (class HasUUID)
import SmartHouse.Algorithm.Edge (Edge)
import SmartHouse.Algorithm.EdgeInfo (EdgeInfo, _line)
import SmartHouse.Algorithm.Ray (Ray, ray)
import Three.Math.Vector (class Vector, Vector3, mkVec3, normal, vecX, vecY, (<**>), (<+>), (<.>))
import Type.Proxy (Proxy(..))


-- | data value used to pass vertice and edge slope info to the skeleton algorithm.
newtype VertWithSlope = VertWithSlope {
    id       :: UUID,
    position :: Vector3,
    slope    :: Angle
}

derive instance Newtype VertWithSlope _
derive instance Generic VertWithSlope _
instance Show VertWithSlope where
    show = genericShow
instance HasUUID VertWithSlope where
    idLens = _id

vertWithSlope :: Vector3 -> Angle -> Effect VertWithSlope
vertWithSlope v slope = do
    i <- genUUID
    pure $ VertWithSlope { id : i, position : v, slope : slope }

updateSlope :: Angle -> VertWithSlope -> VertWithSlope
updateSlope = set _slope

-- intermediete data structure for constructing initial vertices and edges
newtype VertInfo = VertInfo {
    id        :: UUID,

    position  :: Vector3,

    edge      :: Maybe Edge,

    isReflex  :: Boolean,
    bisector  :: Ray,
    usable    :: Boolean,

    leftEdge  :: EdgeInfo,
    rightEdge :: EdgeInfo
    }

derive instance newtypeVertInfo :: Newtype VertInfo _
instance HasUUID VertInfo where
    idLens = _id

_isReflex :: forall t a r. Newtype t { isReflex :: a | r } => Lens' t a
_isReflex = _Newtype <<< prop (Proxy :: Proxy "isReflex")

_bisector :: forall t a r. Newtype t { bisector :: a | r } => Lens' t a
_bisector = _Newtype <<< prop (Proxy :: Proxy "bisector")

_usable :: forall t a r. Newtype t { usable :: a | r } => Lens' t a
_usable = _Newtype <<< prop (Proxy :: Proxy "usable")

_cross :: forall v. Vector v => v -> v -> Number
_cross v1 v2 = vecX v1 * vecY v2 - vecX v2 * vecY v1


is90 :: Angle -> Boolean
is90 a = abs (degreeVal a - 90.0) < 0.1

-- | calculate the bisector vector of two edges with their slopes
calcDir :: Vector3 -> Angle -> Vector3 -> Angle -> Boolean -> Tuple Vector3 Boolean
calcDir leftV leftSlope rightV rightSlope isReflex
    | is90 leftSlope && is90 rightSlope = Tuple (mkVec3 0.0 0.0 1.0) true
    | is90 leftSlope  = Tuple (leftV <**> if isReflex then -1.0 else 1.0) true
    | is90 rightSlope = Tuple (rightV <**> if isReflex then -1.0 else 1.0) true
    | otherwise =
        let lt = tan leftSlope
            rt = tan rightSlope
        
            dir = (leftV <**> lt <+> rightV <**> rt) <**> (if isReflex then -1.0 else 1.0)
            a = acos $ leftV <.> rightV
            -- the bisector is usable only if the two edges are not parallel
            -- based on the angle between the edges are not smaller than 2 degrees
            usable = abs (degreeVal a - 90.0) < 2.0
        in Tuple dir usable

vertInfoFrom :: UUID -> Vector3 -> Maybe Edge -> EdgeInfo -> EdgeInfo -> Maybe Vector3 -> Maybe Vector3 -> VertInfo
vertInfoFrom i p e leftEdge rightEdge vecL vecR =
    let leftVec  = direction (leftEdge ^. _line) <**> (-1.0)
        rightVec = direction $ rightEdge ^. _line
        lv       = fromMaybe leftVec $ normal <$> vecL
        rv       = fromMaybe rightVec $ normal <$> vecR
        isReflex = _cross lv rv > 0.0

        lSlope   = leftEdge  ^. _slope
        rSlope   = rightEdge ^. _slope

        Tuple dir usable = calcDir leftVec lSlope rightVec rSlope isReflex
    in VertInfo {
        id        : i,
        position  : p,

        edge      : e,

        isReflex  : isReflex,
        bisector  : ray p dir,
        usable    : usable,

        leftEdge  : leftEdge,
        rightEdge : rightEdge
        }
