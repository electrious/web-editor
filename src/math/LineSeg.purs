module Math.LineSeg where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst)
import Math.Angle (Angle)
import Math.Utils (lineIntersection)
import Three.Math.Vector (class Vector, Vector2, Vector3, addScaled, angleBetween, cross, mkVec3, normal, toVec2, toVec3, (<**>), (<+>), (<->), (<.>))
import Three.Math.Vector as V

newtype LineSeg v = LineSeg {
  start :: v,
  end   :: v
  }

derive instance newtypeLineSeg :: Newtype (LineSeg v) _
derive instance genericLineSeg :: Generic (LineSeg v) _
instance showLineSeg :: Show v => Show (LineSeg v) where
    show = genericShow
instance eqLineSeg :: Eq v => Eq (LineSeg v) where
    eq (LineSeg { start: s1, end: e1}) (LineSeg { start: s2, end: e2 }) = (s1 == s2 && e1 == e2) || (s1 == e2 && e1 == s2)
instance ordLineSeg :: Ord v => Ord (LineSeg v) where
    compare (LineSeg { start: s1, end: e1}) (LineSeg { start: s2, end: e2 }) = compare s1 s2 <> compare e1 e2
instance functorLineSeg :: Functor LineSeg where
    map f (LineSeg { start: sl, end: el}) = LineSeg { start: f sl, end: f el }

_start :: forall t a r. Newtype t { start :: a | r } => Lens' t a
_start = _Newtype <<< prop (SProxy :: SProxy "start")

_end :: forall t a r. Newtype t { end :: a | r } => Lens' t a
_end = _Newtype <<< prop (SProxy :: SProxy "end")

mkLineSeg :: forall v. v -> v -> LineSeg v
mkLineSeg v1 v2 = LineSeg { start : v1, end : v2 }

-- get vector of the line
lineVec :: forall v. Vector v => LineSeg v -> v
lineVec l = l ^. _end <-> l ^. _start

direction :: forall v. Vector v => LineSeg v -> v
direction = normal <<< lineVec

-- get center of the line
lineCenter :: forall v. Vector v => LineSeg v -> v
lineCenter l = (l ^. _start <+> l ^. _end) <**> 0.5

length :: forall v. Vector v => LineSeg v -> Number
length = V.length <<< lineVec

-- angle between two lines
linesAngle :: forall v. Vector v => LineSeg v -> LineSeg v -> Angle
linesAngle l1 l2 = angleBetween (lineVec l1) (lineVec l2)

-- get line points
linePoints :: forall v. LineSeg v -> Array v
linePoints l = [l ^. _start, l ^. _end]

-- find the most parallel line to a target line in a list of lines
mostParaLineSeg :: forall f v. Foldable f => Vector v => LineSeg v -> f (LineSeg v) -> Maybe (Tuple (LineSeg v) Angle)
mostParaLineSeg target = foldl f Nothing
    where f Nothing l = Just $ Tuple l (linesAngle target l)
          f lv@(Just (Tuple _ la)) l =
              let na = linesAngle l target
              in if na < la then Just (Tuple l na) else lv

-- project point based on the vector line
projPointWithLineSeg :: forall v. Vector v => v -> v -> LineSeg v -> v
projPointWithLineSeg sp p l = sp <+> (lv <**> s)
    where lv  = normal $ lineVec l
          sv  = p <-> sp
          s   = (lv <.> sv) / V.length lv


-- all line segs for a list of poly line vertices (not polygon edges)
lineSegsFromPolyVerts :: forall f v. Foldable f => f v -> List (LineSeg v)
lineSegsFromPolyVerts = fst <<< foldl f (Tuple Nil Nothing)
    where f (Tuple r Nothing) v = Tuple r (Just v)
          f (Tuple r (Just lv)) v = Tuple (mkLineSeg v lv : r) (Just v)

-- perpendicular line of the specified line l, crossing start point of l, in 2D
perpendicularLineSeg :: LineSeg Vector2 -> LineSeg Vector2
perpendicularLineSeg l = mkLineSeg ns ne
    where s = l ^. _start
          e = l ^. _end
          v = toVec3 (e <-> s) 0.0
          vt = mkVec3 0.0 0.0 1.0
          nv = toVec2 $ cross v vt

          ns = addScaled s nv (-20.0)
          ne = addScaled s nv 20.0

-- calculate the perpendicular distance between point p to line l
distToLineSeg :: forall v. Vector v => v -> LineSeg v -> Number
distToLineSeg p l = let lv = normal $ lineVec l
                        sv = (l ^. _start) <-> p
                 in V.length $ sv <-> (lv <**> (sv <.> lv))

-- 2D line intersection point
intersection :: LineSeg Vector3 -> LineSeg Vector3 -> Maybe Vector3
intersection (LineSeg { start: s1, end: e1 })
             (LineSeg { start: s2, end: e2 }) = lineIntersection s1 e1 s2 e2
