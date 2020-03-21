module Math.Angle where

import Prelude

import Math as M

newtype Angle = Angle Number

derive instance eqAngle :: Eq Angle
derive instance ordAngle :: Ord Angle

instance showAngle :: Show Angle where
  show (Angle a) = "Angle(" <> show a <> " deg)"

instance semiringAngle :: Semiring Angle where
  add (Angle a) (Angle b) = Angle (a + b)
  zero = Angle zero
  mul (Angle a) (Angle b) = Angle (a * b)
  one = Angle one

instance ringAngle :: Ring Angle where
  sub (Angle a) (Angle b) = Angle (a - b)

degree :: Number -> Angle
degree d = Angle d

radian :: M.Radians -> Angle
radian r = Angle (r * 180.0 / M.pi)

degreeVal :: Angle -> Number
degreeVal (Angle deg) = deg

radianVal :: Angle -> M.Radians
radianVal (Angle deg) = deg * M.pi / 180.0

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
