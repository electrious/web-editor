module Math.Line where

import Data.Foldable (class Foldable, foldl)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Math.Angle (Angle)
import Model.Roof.RoofPlate (angleBetween)
import Prelude ((/), (<), (<<<))
import Three.Math.Vector (class Vector, length, (<**>), (<+>), (<->), (<.>))

newtype Line v = Line {
  start :: v,
  end   :: v
  }

derive instance newtypeLine :: Newtype (Line v) _

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
mostParaLine :: forall f v. Foldable f => Vector v => Line v -> f (Line v) -> Maybe (Line v)
mostParaLine target = foldl f Nothing
    where f Nothing l = Just l
          f (Just ll) l = if linesAngle l target < linesAngle ll target
                          then Just l
                          else Just ll

-- project point based on the vector line
projPointWithLine :: forall v. Vector v => v -> v -> Line v -> v
projPointWithLine sp p l = sp <+> (lv <**> s)
    where lv  = lineVec l
          sv  = p <-> sp
          s   = (lv <.> sv) / length lv
