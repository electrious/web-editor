module Model.Roof.RoofPlateTransform where

import Prelude hiding (degree)

import Algorithm.PointInPolygon (pointInPolygon)
import Data.Array ((!!))
import Data.Default (class Default, def)
import Data.Foldable (class Foldable, foldl, all)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (^.), (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), filter, fromFoldable, head, null, snoc, tail, zipWith)
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), snd)
import Editor.Common.Lenses (_center, _normal)
import Editor.RoofEditor (toVec2)
import Math (abs, atan, sqrt)
import Math.Angle (Angle, cos, degree, degreeVal, radian, sin)
import Model.Roof.RoofPlate (Polygon, RoofPlate, _borderPoints, _coefs, _rotation, getRoofPolygon)
import Three.Math.Vector (Vector3, cross, length, mkVec3, normal, vecX, vecY, vecZ, (<**>), (<+>), (<->), (<.>))

-- data type representing the right and top vectors for a roof plate
newtype RoofPlateTransform = RoofPlateTransform {
    center :: Vector3,
    right  :: Vector3,
    top    :: Vector3
}

derive instance newtypeRoofPlateTransform :: Newtype RoofPlateTransform _
derive instance genericRoofPlateTransform :: Generic RoofPlateTransform _
instance showRoofPlateTransform :: Show RoofPlateTransform where
    show = genericShow
instance defaultRoofPlateTransform :: Default RoofPlateTransform where
    def = RoofPlateTransform {
              center : def,
              right  : def,
              top    : def
          }

_right :: forall t a r. Newtype t { right :: a | r } => Lens' t a
_right = _Newtype <<< prop (SProxy :: SProxy "right")

_top :: forall t a r. Newtype t { top :: a | r } => Lens' t a
_top = _Newtype <<< prop (SProxy :: SProxy "top")

-- | transform a point in roofplate's local coordinate to global coordinate
transformVector :: RoofPlateTransform -> Vector3 -> Vector3
transformVector t v = c <+> r <**> (vecX v) <+> top <**> (vecY v)
    where c = t ^. _center
          r = t ^. _right
          top = t ^. _top

transformVectors :: forall f. Functor f => RoofPlateTransform -> f Vector3 -> f Vector3
transformVectors t = map (transformVector t)


degreeThreshold :: Angle
degreeThreshold = degree 10.0

lengthThreshold :: Number
lengthThreshold = 3.0

getRoofTransform :: RoofPlate -> RoofPlateTransform
getRoofTransform r = if abs rotOverride > 0.1 || null candidates || absIncline incAxisVec < degreeThreshold
                     then getRotationOverrideVecs r
                     else def # _center .~ r ^. _center
                              # _right  .~ normal groundAxisVec
                              # _top    .~ normal incAxisVec
    where rotOverride = degreeVal $ r ^. _rotation
          validLine l = absIncline l < degreeThreshold && length l > lengthThreshold
          candidates = filter validLine $ lineVectors (r ^. _borderPoints)

          -- find the longest line in candidates
          groundAxis = longestVector candidates
          roofNormal = r ^. _normal
          normVec = if vecZ roofNormal < 0.0 then roofNormal <**> (-1.0) else roofNormal
          incAxis = cross normVec groundAxis
          incAxisVec = if vecZ incAxis < 0.0 then incAxis <**> (-1.0) else incAxis
          groundAxisVec = if normVec <.> cross groundAxis incAxisVec < 0.0
                          then groundAxis <**> (-1.0)
                          else groundAxis
          


getRotationOverrideVecs :: RoofPlate -> RoofPlateTransform
getRotationOverrideVecs r = def # _center .~ center
                                # _right  .~ normal nx 
                                # _top    .~ normal ny
    where plane = r ^. _coefs
          p0 = fromMaybe 0.0 (plane !! 0)
          p1 = fromMaybe 1.0 (plane !! 1)
          p2 = fromMaybe 0.0 (plane !! 2)
          p3 = fromMaybe 0.0 (plane !! 3)
          zVector = mkVec3 p0 p1 p2
          
          center = r ^. _center
          xx = vecX center + 1.0
          zz = vecZ center
          yy = (-p0 * xx - p2 * zz - p3) / p1

          -- red vector
          xv = mkVec3 1.0 (yy - vecY center) 0.0

          -- blue vector
          yv = cross xv zVector
          yVector = if vecZ yv < 0.0 then yv <**> (-1.0) else yv

          -- compute red cross blue, take dot product with green and make sure it's positive
          -- othewise take negation of red vector
          tmp = cross xv yVector <.> zVector
          xVector = if tmp < 0.0 then xv <**> (-1.0) else xv

          -- apply the overrided rotation
          theta = r ^. _rotation
          theta2 = theta + degree 90.0

          nx = xVector <**> cos theta <+> yVector <**> sin theta
          ny = xVector <**> cos theta2 <+> yVector <**> sin theta2


-- helper functions

-- calculate the absolute incline angle of a vector
absIncline :: Vector3 -> Angle
absIncline v = radian $ abs $ atan $ vecZ v / mag
    where mag = sqrt (x * x + y * y)
          x = vecX v
          y = vecY v

-- calculate the largest line in a list of line vectors
longestVector :: forall f. Foldable f => f Vector3 -> Vector3
longestVector = snd <<< foldl f (Tuple 0.0 def)
    where f (Tuple len ov) v = let vl = length v
                              in if vl > len
                                 then Tuple vl v
                                 else Tuple len ov

-- calculate a list of lines based on a list of point vectors
lineVectors :: forall f. Foldable f => f Vector3 -> List Vector3
lineVectors ps = zipWith (<->) pl npl
    where pl = fromFoldable ps
          npl = fromMaybe Nil $ snoc <$> tail pl <*> head pl


-- \ check if a polygon contains a point inside.
wrapAroundPoint :: Polygon -> RoofPlateTransform -> Vector3 -> Boolean
wrapAroundPoint poly t = pointInPolygon poly <<< toVec2 <<< transformVector t

-- | check if a Roofplate contains all points in a list. The Points should be in
-- roofplate's local coordinate
wrapAroundPoints :: forall f. Foldable f => RoofPlate -> f Vector3 -> Boolean
wrapAroundPoints roof = all (wrapAroundPoint poly t)
    where poly = getRoofPolygon roof
          t    = getRoofTransform roof
