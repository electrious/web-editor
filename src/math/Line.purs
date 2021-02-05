module Math.Line where

import Data.Eq (class Eq)
import Data.Foldable (class Foldable, foldl)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Math.Angle (Angle)
import Model.Roof.RoofPlate (angleBetween)
import Prelude (($), (&&), (/), (<), (<<<), (==), (||))
import Three.Math.Vector (class Vector, length, normal, (<**>), (<+>), (<->), (<.>))

newtype Line v = Line {
  start :: v,
  end   :: v
  }

derive instance newtypeLine :: Newtype (Line v) _
instance eqLine :: Eq v => Eq (Line v) where
    eq (Line { start: s1, end: e1}) (Line { start: s2, end: e2 }) = (s1 == s2 && e1 == e2) || (s1 == e2 && e1 == s2)

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
