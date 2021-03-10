module Math.Line where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Math.Angle (Angle)
import Model.Roof.RoofPlate (angleBetween)
import Three.Math.Vector (class Vector, Vector2, addScaled, cross, length, mkVec2, mkVec3, normal, toVec2, toVec3, vecX, vecY, (<**>), (<+>), (<->), (<.>))

newtype Line v = Line {
  start :: v,
  end   :: v
  }

derive instance newtypeLine :: Newtype (Line v) _
derive instance genericLine :: Generic (Line v) _
instance showLine :: Show v => Show (Line v) where
    show = genericShow
instance eqLine :: Eq v => Eq (Line v) where
    eq (Line { start: s1, end: e1}) (Line { start: s2, end: e2 }) = (s1 == s2 && e1 == e2) || (s1 == e2 && e1 == s2)
instance functorLine :: Functor Line where
    map f (Line { start: sl, end: el}) = Line { start: f sl, end: f el }

_start :: forall t a r. Newtype t { start :: a | r } => Lens' t a
_start = _Newtype <<< prop (SProxy :: SProxy "start")

_end :: forall t a r. Newtype t { end :: a | r } => Lens' t a
_end = _Newtype <<< prop (SProxy :: SProxy "end")

mkLine :: forall v. v -> v -> Line v
mkLine v1 v2 = Line { start : v1, end : v2 }

-- get vector of the line
lineVec :: forall v. Vector v => Line v -> v
lineVec l = l ^. _end <-> l ^. _start

-- get center of the line
lineCenter :: forall v. Vector v => Line v -> v
lineCenter l = (l ^. _start <+> l ^. _end) <**> 0.5

lineLength :: forall v. Vector v => Line v -> Number
lineLength = length <<< lineVec

-- angle between two lines
linesAngle :: forall v. Vector v => Line v -> Line v -> Angle
linesAngle l1 l2 = angleBetween (lineVec l1) (lineVec l2)

-- get line points
linePoints :: forall v. Line v -> Array v
linePoints l = [l ^. _start, l ^. _end]

-- find the most parallel line to a target line in a list of lines
mostParaLine :: forall f v. Foldable f => Vector v => Line v -> f (Line v) -> Maybe (Tuple (Line v) Angle)
mostParaLine target = foldl f Nothing
    where f Nothing l = Just $ Tuple l (linesAngle target l)
          f lv@(Just (Tuple ll la)) l =
              let na = linesAngle l target
              in if na < la then Just (Tuple l na) else lv

-- project point based on the vector line
projPointWithLine :: forall v. Vector v => v -> v -> Line v -> v
projPointWithLine sp p l = sp <+> (lv <**> s)
    where lv  = normal $ lineVec l
          sv  = p <-> sp
          s   = (lv <.> sv) / length lv


-- perpendicular line of the specified line l, crossing start point of l, in 2D
perpendicularLine :: Line Vector2 -> Line Vector2
perpendicularLine l = mkLine ns ne
    where s = l ^. _start
          e = l ^. _end
          v = toVec3 (e <-> s) 0.0
          vt = mkVec3 0.0 0.0 1.0
          nv = toVec2 $ cross v vt

          ns = addScaled s nv (-20.0)
          ne = addScaled s nv 20.0

-- calculate the perpendicular distance between point p to line l
distToLine :: forall v. Vector v => v -> Line v -> Number
distToLine p l = let lv = normal $ lineVec l
                     sv = (l ^. _start) <-> p
                 in length $ sv <-> (lv <**> (sv <.> lv))

-- 2D line intersection point
intersection :: Line Vector2 -> Line Vector2 -> Vector2
intersection (Line { start: s1, end: e1 }) (Line { start: s2, end: e2 }) = mkVec2 x y
    where x1 = vecX s1
          y1 = vecY s1
          x2 = vecX e1
          y2 = vecY e1
          x3 = vecX s2
          y3 = vecY s2
          x4 = vecX e2
          y4 = vecY e2
          d = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
          n1 = x1 * y2 - y1 * x2
          n2 = x3 * y4 - y3 * x4
          x = (n1 * (x3 - x4) - (x1 - x2) * n2) / d
          y = (n1 * (y3 - y4) - (y1 - y2) * n2) / d
