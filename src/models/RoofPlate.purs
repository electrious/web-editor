module Models.RoofPlate where

import Prelude hiding (degree)

import Data.Array ((..))
import Data.Array as Arr
import Data.Enum (class BoundedEnum, class Enum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (fromMaybe)
import Data.UUID (genUUID, toString)
import Effect (Effect)
import Math as Math
import Math.Angle (Angle, acos, atan2, degree, degreeVal, radian)
import Three.Math.Vector (class Vector, Vector2, Vector3, addScaled, cross, length, mkVec2, mkVec3, vecX, vecY, vecZ, (<.>))

-- | define the Orientation data and instances for common typeclasses
data Orientation = Landscape | Portrait

derive instance genericOrient :: Generic Orientation _
derive instance eqOrient :: Eq Orientation
derive instance ordOrient :: Ord Orientation

instance boundedOrient :: Bounded Orientation where
    top = genericTop
    bottom = genericBottom

instance enumOrient :: Enum Orientation where
    succ = genericSucc
    pred = genericPred

instance boundenumOrient :: BoundedEnum Orientation where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum

instance showOrient :: Show Orientation where
    show = genericShow

-- | define the Alignment type and instances for common typeclasses
data Alignment = Grid | Brick

derive instance eqAlignment :: Eq Alignment
derive instance ordAlighment :: Ord Alignment
derive instance genericAlignment :: Generic Alignment _

instance enumAlignment :: Enum Alignment where
    succ = genericSucc
    pred = genericPred

instance boundedAlignment :: Bounded Alignment where
    top = genericTop
    bottom = genericBottom

instance boundedEnumALignment :: BoundedEnum Alignment where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum

instance showAlignment :: Show Alignment where
    show = genericShow

-- | define the core RoofPlate type as a record
type RoofPlate = {
    id           :: String,
    intId        :: Int,
    leadId       :: Int,
    borderPoints :: Array Vector3,
    coefs        :: Array Number,
    center       :: Vector3,
    normal       :: Vector3,
    orientation  :: Orientation,
    alignment    :: Alignment,
    slope        :: Angle,
    azimuth      :: Angle,
    rotation     :: Angle
}

-- | external JSRoofPlate model used in JS code. The data received from user and
-- updates sent back to user should be in this format
type JSRoofPlate = {
    uuid              :: String,
    id                :: Int,
    lead_id           :: Int,
    border_points     :: Array { x :: Number, y :: Number, z :: Number },
    coefs             :: Array Number,
    center            :: Array Number,
    normal            :: Array Number,
    orientation       :: Int,
    alignment         :: Int,
    slope             :: Number,
    azimuth           :: Number,
    rotation_override :: Number
}

arrVec :: Array Number -> Vector3
arrVec [x, y, z] = mkVec3 x y z
arrVec _ = mkVec3 0.0 0.0 0.0

-- | Convert external JSroofPlate to internal RoofPlate
fromJSRoofPlate :: JSRoofPlate -> RoofPlate
fromJSRoofPlate r = {
    id           : r.uuid,
    intId        : r.id,
    leadId       : r.lead_id,
    borderPoints : (\v -> mkVec3 v.x v.y v.z) <$> r.border_points,
    coefs        : r.coefs,
    center       : arrVec r.center,
    normal       : arrVec r.normal,
    orientation  : fromMaybe Landscape (toEnum r.orientation),
    alignment    : fromMaybe Grid (toEnum r.alignment),
    slope        : degree r.slope,
    azimuth      : degree r.azimuth,
    rotation     : degree r.rotation_override
}

-- | 2D polygon for roof plate projection on ground
type Polygon = Array Vector2

-- | get the 2D polygon for a roof plate
getRoofPolygon :: RoofPlate -> Polygon
getRoofPolygon r = f <$> r.borderPoints
    where f v = mkVec2 (vecX v) (vecY v)

-- | helper function to calculate angle between two Vector3
angleBetween :: forall a. Vector a => a -> a -> Angle
angleBetween v1 v2 = acos $ d / (length v1 * length v2)
    where d = v1 <.> v2

-- | calculate the gutter vector based on roof normal vector
-- the gutter vector always has 0 for the z element, and it should be
-- perpendicular to the normal vector.
-- Assume normal vector to be (nx, ny, nz)
-- and let the gutter vector to be (x, y, 0).
-- First, the dot product should be 0, so 'nx * x + ny * y + nz * 0 = 0'
-- Second, let it be normalized vector, so 'x * x + y * y + 0 = 1'
-- solve the two equations and get the x,y (we only need one solution, and
-- we'll take the positive x here.)
gutterVector :: Vector3 -> Vector3
gutterVector normal = mkVec3 x y 0.0
    where nx = vecX normal
          ny = vecY normal
          c = nx / ny
          x = Math.sqrt(1.0 / (1.0 + c * c))
          y = -x * c

-- | get the rafter vector based on normal vector and gutter vector
rafterVector :: Vector3 -> Vector3 -> Vector3
rafterVector = cross

-- | create default border points
defBorderPoints :: Vector3 -> Vector3 -> Vector3 -> Array Vector3
defBorderPoints center gutter rafter = [p1, p2, p3, p4, p1]
    where m1 = addScaled center gutter 2.0
          m2 = addScaled center gutter (-2.0)
          p1 = addScaled m1 rafter 2.0
          p2 = addScaled m1 rafter (-2.0)
          p3 = addScaled m2 rafter (-2.0)
          p4 = addScaled m2 rafter 2.0

-- | calculate the azimuth angle based on roof normal
getAzimuth :: Vector3 -> Angle
getAzimuth normal = if a > 360.0 then degree (a - 360.0)
                    else if a < 0.0 then degree (a + 360.0)
                    else ang
    where ang = atan2 (vecX normal) (vecY normal)
          a = degreeVal ang

-- | create a new RoofPlate based on the start position and normal vector
newRoofPlate :: Vector3 -> Vector3 -> Effect RoofPlate
newRoofPlate center normal = do
    u <- genUUID
    let -- normal vector projection on ground
        projN = mkVec3 (vecX normal) (vecY normal) 0.0
        angle = angleBetween normal projN

        slope = degree (90.0 - degreeVal angle)
        azimuth = getAzimuth normal

        gutter = gutterVector normal
        rafter = rafterVector normal gutter
        borderPoints = defBorderPoints center gutter rafter

    pure {
        id           : toString u,
        intId        : 0,
        leadId       : 0,
        borderPoints : borderPoints,
        coefs        : [],
        center       : center,
        normal       : normal,
        orientation  : Landscape,
        alignment    : Brick,
        slope        : slope,
        azimuth      : azimuth,
        rotation     : degree 0.0
    }

-- | Types of operations applied to roofs
data RoofOperation = RoofOpCreate RoofPlate
                   | RoofOpDelete String
                   | RoofOpUpdate RoofPlate

derive instance genericRoofOp :: Generic RoofOperation _
derive instance eqRoofOp :: Eq RoofOperation

instance showRoofOp :: Show RoofOperation where
    show = genericShow


-- | Point defines a 3D point used in exported RoofEdited value
type Point = {
    x :: Number,
    y :: Number,
    z :: Number
}

vec2Point :: Vector3 -> Point
vec2Point v = { x: vecX v, y: vecY v, z: vecZ v }

-- | RoofEdited defines data required by the final API to actually
-- update the roofpaltes on server
type RoofEdited = {
    ground   :: Point,
    inclined :: Point,
    contours :: Array Point,
    indices  :: Array Int
}

toRoofEdited :: RoofPlate -> RoofEdited
toRoofEdited r = { ground: vec2Point gutter,
                   inclined: vec2Point rafter,
                   contours: vec2Point <$> r.borderPoints,
                   indices: 1..(Arr.length r.borderPoints)
                 }
    where gutter = gutterVector r.normal
          rafter = rafterVector r.normal gutter
