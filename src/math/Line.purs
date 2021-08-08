module Math.Line where

import Prelude

import Data.Filterable (filter)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Math.Angle (degreeVal)
import Math.LineSeg (LineSeg, _start, direction)
import Math.Utils (lineIntersection, zeroZ)
import Three.Math.Vector (class Vector, Vector3, angleBetween, length, normal, (<**>), (<+>), (<->), (<.>))
import Type.Proxy (Proxy(..))

newtype Line v = Line {
    origin    :: v,
    direction :: v
    }

derive instance newtypeLine :: Newtype (Line v) _
derive instance genericLine :: Generic (Line v) _
derive instance eqLine :: Eq v => Eq (Line v)
instance showLine :: Show v => Show (Line v) where
    show = genericShow
    
_origin :: forall t a r. Newtype t { origin :: a | r } => Lens' t a
_origin = _Newtype <<< prop (Proxy :: Proxy "origin")

_direction :: forall t a r. Newtype t { direction :: a | r } => Lens' t a
_direction = _Newtype <<< prop (Proxy :: Proxy "direction")

line :: forall v. v -> v -> Line v
line o d = Line { origin : o, direction : d }

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


sameDir :: Vector3 -> Vector3 -> Boolean
sameDir a b = degreeVal (angleBetween (normal a) (normal b)) < 1.0

-- make sure the lines are 2D and the intersection point should in the same direction
-- of both lines to avoid incorrect intersection points
intersection :: Line Vector3 -> Line Vector3 -> Maybe Vector3
intersection l1 l2 = filter f i
    where s1 = zeroZ $ l1 ^. _origin
          e1 = s1 <+> l1 ^. _direction
          s2 = zeroZ $ l2 ^. _origin
          e2 = s2 <+> l2 ^. _direction
          i = lineIntersection s1 e1 s2 e2
          f p = sameDir (p <-> s1) (l1 ^. _direction) && sameDir (p <-> s2) (l2 ^. _direction)
