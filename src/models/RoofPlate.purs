module Models.RoofPlate where

import Prelude hiding (degree)

import Data.Array ((..))
import Data.Array as Arr
import Data.Enum (toEnum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.UUID (genUUID, toString)
import Effect (Effect)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)
import Math as Math
import Math.Angle (Angle, acos, atan2, degree, degreeVal)
import Models.Panel (Alignment(..), Orientation(..))
import Three.Math.Vector (class Vector, Vector2, Vector3, addScaled, cross, length, mkVec2, mkVec3, vecX, vecY, vecZ, (<.>))

-- | define the core RoofPlate type as a record
newtype RoofPlate = RoofPlate {
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

derive instance newtypeRoofplate :: Newtype RoofPlate _
derive instance genericRoofplate :: Generic RoofPlate _
instance showRoofplate :: Show RoofPlate where
    show = genericShow
instance eqRoofplate :: Eq RoofPlate where
    eq = genericEq

_roofId :: Lens' RoofPlate String
_roofId = _Newtype <<< prop (SProxy :: SProxy "id")

_leadId :: Lens' RoofPlate Int
_leadId = _Newtype <<< prop (SProxy :: SProxy "leadId")

_borderPoints :: Lens' RoofPlate (Array Vector3)
_borderPoints = _Newtype <<< prop (SProxy :: SProxy "borderPoints")

_center :: Lens' RoofPlate Vector3
_center = _Newtype <<< prop (SProxy :: SProxy "center")

_normal :: Lens' RoofPlate Vector3
_normal = _Newtype <<< prop (SProxy :: SProxy "normal")

_orientation :: Lens' RoofPlate Orientation
_orientation = _Newtype <<< prop (SProxy :: SProxy "orientation")

_alignment :: Lens' RoofPlate Alignment
_alignment = _Newtype <<< prop (SProxy :: SProxy "alignment")

_slope :: Lens' RoofPlate Angle
_slope = _Newtype <<< prop (SProxy :: SProxy "slope")

_azimuth :: Lens' RoofPlate Angle
_azimuth = _Newtype <<< prop (SProxy :: SProxy "azimuth")

_rotation :: Lens' RoofPlate Angle
_rotation = _Newtype <<< prop (SProxy :: SProxy "rotation")


-- | Point defines a 3D point used in external values
newtype Point = Point {
    x :: Number,
    y :: Number,
    z :: Number
}

derive instance newtypePoint :: Newtype Point _
derive instance genericPoint :: Generic Point _
instance showPoint :: Show Point where
    show = genericShow
instance encodePoint :: Encode Point where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
instance decodePoint :: Decode Point where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

vec2Point :: Vector3 -> Point
vec2Point v = Point { x: vecX v, y: vecY v, z: vecZ v }

point2Vec :: Point -> Vector3
point2Vec (Point { x, y, z}) = mkVec3 x y z


newtype UnifiedPoint = UnifiedPoint {
    x      :: Number,
    y      :: Number,
    z      :: Number,
    shade  :: Number,
    rating :: Number
}

derive instance genericUnifiedPoint :: Generic UnifiedPoint _
instance showUnifiedPoint :: Show UnifiedPoint where
    show = genericShow
instance encodeUnifiedPoint :: Encode UnifiedPoint where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
instance decodeUnifiedPoint :: Decode UnifiedPoint where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

-- | external JSRoofPlate model used in JS code. The data received from user and
-- updates sent back to user should be in this format
newtype JSRoofPlate = JSRoofPlate {
    id                :: Int,
    uuid              :: String,
    created_at        :: String,
    lead_id           :: Int,
    border_points     :: Array Point,
    unified_points    :: Maybe (Array UnifiedPoint),
    orientation       :: Int,
    alignment         :: Int,
    slope             :: Number,
    coefs             :: Array Number,
    center            :: Array Number,
    normal            :: Array Number,
    azimuth           :: Number,
    rotation_override :: Number
}

derive instance genericJSRoofPlate :: Generic JSRoofPlate _
instance showJSRoofPlate :: Show JSRoofPlate where
    show = genericShow
instance encodeJSRoofPlate :: Encode JSRoofPlate where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
instance decodeJSRoofPlate :: Decode JSRoofPlate where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

arrVec :: Array Number -> Vector3
arrVec [x, y, z] = mkVec3 x y z
arrVec _ = mkVec3 0.0 0.0 0.0

-- | Convert external JSRoofPlate to internal RoofPlate
fromJSRoofPlate :: JSRoofPlate -> RoofPlate
fromJSRoofPlate (JSRoofPlate r) = RoofPlate {
    id           : r.uuid,
    intId        : r.id,
    leadId       : r.lead_id,
    borderPoints : point2Vec <$> r.border_points,
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
getRoofPolygon r = f <$> r ^. _borderPoints
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

    pure $ RoofPlate {
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


-- | RoofEdited defines data required by the final API to actually
-- update the roofpaltes on server
newtype RoofEdited = RoofEdited {
    ground   :: Point,
    inclined :: Point,
    contours :: Array Point,
    indices  :: Array Int
}

derive instance newtypeRoofEdited :: Newtype RoofEdited _
derive instance genericRoofEdited :: Generic RoofEdited _
instance showRoofEdited :: Show RoofEdited where
    show = genericShow
instance encodeRoofEdited :: Encode RoofEdited where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })

_ground :: Lens' RoofEdited Point
_ground = _Newtype <<< prop (SProxy :: SProxy "ground")

_inclined :: Lens' RoofEdited Point
_inclined = _Newtype <<< prop (SProxy :: SProxy "inclined")

_contours :: Lens' RoofEdited (Array Point)
_contours = _Newtype <<< prop (SProxy :: SProxy "contours")

_indices :: Lens' RoofEdited (Array Int)
_indices = _Newtype <<< prop (SProxy :: SProxy "indices")

toRoofEdited :: RoofPlate -> RoofEdited
toRoofEdited r = RoofEdited {
                   ground   : vec2Point gutter,
                   inclined : vec2Point rafter,
                   contours : vec2Point <$> r ^. _borderPoints,
                   indices  : 1..(Arr.length $ r ^. _borderPoints)
                 }
    where gutter = gutterVector $ r ^. _normal
          rafter = rafterVector (r ^. _normal) gutter
