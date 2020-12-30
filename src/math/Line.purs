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
import Three.Math.Vector (Vector3, length, (<**>), (<+>), (<->), (<.>))

newtype Line = Line {
  start :: Vector3,
  end   :: Vector3
  }

derive instance newtypeLine :: Newtype Line _

_start :: forall t a r. Newtype t { start :: a | r } => Lens' t a
_start = _Newtype <<< prop (SProxy :: SProxy "start")

_end :: forall t a r. Newtype t { end :: a | r } => Lens' t a
_end = _Newtype <<< prop (SProxy :: SProxy "end")

mkLine :: Vector3 -> Vector3 -> Line
mkLine v1 v2 = Line { start : v1, end : v2 }

-- get vector of the line
lineVec :: Line -> Vector3
lineVec l = l ^. _end <-> l ^. _start

-- angle between two lines
linesAngle :: Line -> Line -> Angle
linesAngle l1 l2 = angleBetween (lineVec l1) (lineVec l2)

-- get line points
linePoints :: Line -> Array Vector3
linePoints l = [l ^. _start, l ^. _end]

-- find the most parallel line to a target line in a list of lines
mostParaLine :: forall f. Foldable f => Line -> f Line -> Maybe Line
mostParaLine target = foldl f Nothing
    where f Nothing l = Just l
          f (Just ll) l = if linesAngle l target < linesAngle ll target
                          then Just l
                          else Just ll

-- project point based on the vector line
projPointWithLine :: Vector3 -> Vector3 -> Line -> Vector3
projPointWithLine sp p l = sp <+> (lv <**> s)
    where lv  = lineVec l
          sv  = p <-> sp
          s   = (lv <.> sv) / length lv
