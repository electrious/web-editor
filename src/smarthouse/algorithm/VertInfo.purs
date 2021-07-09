module SmartHouse.Algorithm.VertInfo where

import Prelude

import Data.Default (def)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Type.Proxy (Proxy(..))
import Data.Tuple (Tuple(..))
import Math.LineSeg (LineSeg, direction)
import SmartHouse.Algorithm.Ray (Ray, ray)
import Three.Math.Vector (class Vector, Vector3, normal, length, vecX, vecY, (<**>), (<+>))

-- intermediete data structure for constructing initial vertices and edges
newtype VertInfo = VertInfo {
    position :: Vector3,
    height   :: Number,
    isReflex :: Boolean,
    bisector :: Ray,
    usable   :: Boolean
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


vertInfoFrom :: Vector3 -> Number -> LineSeg Vector3 -> LineSeg Vector3 -> Maybe Vector3 -> Maybe Vector3 -> VertInfo
vertInfoFrom p h leftEdge rightEdge vecL vecR =
    let leftVec  = direction leftEdge <**> (-1.0)
        rightVec = direction rightEdge
        lv       = fromMaybe leftVec $ normal <$> vecL
        rv       = fromMaybe rightVec $ normal <$> vecR
        isReflex = _cross lv rv < 0.0
        Tuple dir usable = checkLength $ (leftVec <+> rightVec) <**> (if isReflex then -1.0 else 1.0)
    in VertInfo {
        position : p,
        height   : h,
        isReflex : isReflex,
        bisector : ray p dir,
        usable   : usable
        }



checkLength :: Vector3 -> Tuple Vector3 Boolean
checkLength v = if length v < 0.1 then Tuple def false else Tuple v true
