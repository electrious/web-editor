module Algorithm.Plane where

import Prelude

import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Lens ((^.))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Editor.Common.Lenses (_normal, _point)
import Math.Line (Line, _direction, _origin, line)
import Three.Math.Vector (Vector3, cross, normal, (<**>), (<+>), (<->), (<.>))

newtype Plane = Plane {
    point  :: Vector3,
    normal :: Vector3
}

derive instance Newtype Plane _
derive instance Generic Plane _
derive instance Eq Plane
instance Default Plane where
    def = Plane {
        point  : def,
        normal : def
    }
instance Show Plane where
    show = genericShow


mkPlane :: Vector3 -> Vector3 -> Plane
mkPlane p n = Plane { point : p, normal : n }

-- | Line Plane intersection algorithm
-- reference: https://en.wikipedia.org/wiki/Line–plane_intersection
data InterRes = NoIntersection            -- line and plane is parallel to each other and no intersection point
              | IntersectPoint Vector3    -- line intersect plane in one point
              | InPlane                   -- line is in the plane directly


intersectLinePlane :: Line Vector3 -> Plane -> InterRes
intersectLinePlane l p =
    let p0 = p ^. _point
        pn = p ^. _normal
        l0 = l ^. _origin
        lv = l ^. _direction
        a = (p0 <-> l0) <.> pn
        b = lv <.> pn
    in if b == 0.0
       then if a == 0.0
            then InPlane
            else NoIntersection
       else IntersectPoint $ l0 <+> lv <**> (a / b)


-- | Plane-Plane intersection algorithm
-- reference: https://en.wikipedia.org/wiki/Plane_(geometry)#Line_of_intersection_between_two_planes
intersect :: Plane -> Plane -> Line Vector3
intersect p1 p2 = line o dir
    where n1  = normal $ p1 ^. _normal
          r1  = p1 ^. _point
          n2  = normal $ p2 ^. _normal
          r2  = p2 ^. _point
          h1  = n1 <.> r1
          h2  = n2 <.> r2
          n12 = n1 <.> n2
          a   = 1.0 - n12 * n12
          c1  = (h1 - h2 * n12) / a
          c2  = (h2 - h1 * n12) / a
          o   = n1 <**> c1 <+> n2 <**> c2
          dir = cross n1 n2
