module Math.Line where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Math.LineSeg (LineSeg, _start, direction)
import Math.Utils (lineIntersection)
import Three.Math.Vector (class Vector, Vector3, length, (<**>), (<+>), (<->), (<.>))

newtype Line v = Line {
    origin    :: v,
    direction :: v
    }

derive instance newtypeLine :: Newtype (Line v) _
derive instance genericLine :: Generic (Line v) _
instance showLine :: Show v => Show (Line v) where
    show = genericShow
    
_origin :: forall t a r. Newtype t { origin :: a | r } => Lens' t a
_origin = _Newtype <<< prop (SProxy :: SProxy "origin")

_direction :: forall t a r. Newtype t { direction :: a | r } => Lens' t a
_direction = _Newtype <<< prop (SProxy :: SProxy "direction")

lineFromSeg :: forall v. Vector v => LineSeg v -> Line v
lineFromSeg s = Line {
    origin    : s ^. _start,
    direction : direction s
    }

distFromPoint :: forall v. Vector v => v -> Line v -> Number
distFromPoint p l =
    let d = p <-> l ^. _origin
        dir = l ^. _direction
    in length $ d <-> (dir <**> (d <.> dir))

intersection :: Line Vector3 -> Line Vector3 -> Maybe Vector3
intersection l1 l2 = lineIntersection s1 e1 s2 e2
    where s1 = l1 ^. _origin
          e1 = s1 <+> l1 ^. _direction
          s2 = l2 ^. _origin
          e2 = s2 <+> l2 ^. _direction
