module Clipper where

import Prelude

import Data.Int (round)
import Effect (Effect)
import Three.Math.Vector (class HasX, class HasY, vecX, vecY)

foreign import data IntPoint :: Type
foreign import mkIntPoint :: Int -> Int -> IntPoint

scale :: Number
scale = 1000.0

vec2IntPoint :: forall v. HasX v => HasY v => v -> IntPoint
vec2IntPoint v = mkIntPoint x y
    where x = round $ vecX v * scale
          y = round $ vecY v * scale

type Path = Array IntPoint
type Paths = Array Path

reversePath :: Path -> Path
reversePath = ffi ["p"] "p.reverse()"

foreign import reversePaths :: Paths -> Paths

foreign import data PolyType :: Type
foreign import ptSubject :: PolyType
foreign import ptClip    :: PolyType

foreign import data ClipType :: Type
foreign import ctIntersection :: ClipType
foreign import ctUnion        :: ClipType
foreign import ctDifference   :: ClipType
foreign import ctXor          :: ClipType

foreign import data PolyFillType :: Type
foreign import pftEvenOdd  :: PolyFillType
foreign import pftNonZero  :: PolyFillType
foreign import pftPositive :: PolyFillType
foreign import pftNegative :: PolyFillType

foreign import data Clipper :: Type
foreign import mkClipper :: Effect Clipper


foreign import addPath :: Path -> PolyType -> Boolean -> Clipper -> Effect Unit
foreign import addPaths :: Paths -> PolyType -> Boolean -> Clipper -> Effect Unit
foreign import clear :: Clipper -> Effect Unit

foreign import area :: Path -> Number
foreign import cleanPolygon :: Path -> Int -> Effect Path
foreign import cleanPolygons :: Paths -> Int -> Effect Paths
foreign import execute :: ClipType -> Effect Paths
foreign import polyOrientation :: Path -> Boolean
foreign import pointInPolygon :: IntPoint -> Path -> Boolean
foreign import simplifyPolygon :: Path -> PolyFillType -> Effect Paths
foreign import simplifyPolygons :: Paths -> PolyFillType -> Effect Paths
