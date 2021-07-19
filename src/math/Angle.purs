module Math.Angle where

import Prelude hiding (degree)

import Data.Default (class Default)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Number as N
import Foreign.Generic (class Decode, class Encode, decode, encode)
import Math as M

newtype Angle = Angle Number

derive instance eqAngle :: Eq Angle
derive instance ordAngle :: Ord Angle
derive instance newtypeAngle :: Newtype Angle _
instance defaultAngle :: Default Angle where
    def = Angle 0.0

instance encodeAngle :: Encode Angle where
    encode (Angle n) = encode n

instance docodeAngle :: Decode Angle where
    decode = map Angle <<< decode

instance showAngle :: Show Angle where
  show (Angle a) = "Angle(" <> show a <> " deg)"

instance semiringAngle :: Semiring Angle where
  add (Angle a) (Angle b) = Angle (a + b)
  zero = Angle zero
  mul (Angle a) (Angle b) = Angle (a * b)
  one = Angle one

instance ringAngle :: Ring Angle where
  sub (Angle a) (Angle b) = Angle (a - b)

instance commutativeRingAngle :: CommutativeRing Angle

degree :: Number -> Angle
degree d = Angle d

radian :: M.Radians -> Angle
radian r = Angle (r * 180.0 / M.pi)

degreeVal :: Angle -> Number
degreeVal (Angle deg) = deg

radianVal :: Angle -> M.Radians
radianVal (Angle deg) = deg * M.pi / 180.0

fromString :: String -> Maybe Angle
fromString = map degree <<< N.fromString

sin :: Angle -> Number
sin = M.sin <<< radianVal

cos :: Angle -> Number
cos = M.cos <<< radianVal

tan :: Angle -> Number
tan = M.tan <<< radianVal

asin :: Number -> Angle
asin = radian <<< M.asin

acos :: Number -> Angle
acos = radian <<< M.acos

atan :: Number -> Angle
atan = radian <<< M.atan

atan2 :: Number -> Number -> Angle
atan2 a1 a2 = radian $ M.atan2 a1 a2
