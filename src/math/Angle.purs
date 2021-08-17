module Math.Angle where

import Prelude hiding (degree)

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Default (class Default)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Number as N
import Math as M

newtype Angle = Angle Number

derive instance Eq Angle
derive instance Ord Angle
derive instance Newtype Angle _
instance Default Angle where
    def = Angle 0.0
instance Show Angle where
  show (Angle a) = "Angle(" <> show a <> " deg)"

instance Semiring Angle where
  add (Angle a) (Angle b) = Angle (a + b)
  zero = Angle zero
  mul (Angle a) (Angle b) = Angle (a * b)
  one = Angle one

instance Ring Angle where
  sub (Angle a) (Angle b) = Angle (a - b)

instance CommutativeRing Angle
instance EncodeJson Angle where
    encodeJson (Angle a) = encodeJson a
instance DecodeJson Angle where
    decodeJson = decodeJson >>> map Angle

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
