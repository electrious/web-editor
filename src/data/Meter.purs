module Data.Meter where

import Prelude

import Data.Newtype (class Newtype)
import Foreign.Generic (class Decode, class Encode, decode, encode)

newtype Meter = Meter Number

derive instance newtypeMeter :: Newtype Meter _
derive instance eqMeter :: Eq Meter
derive instance ordMeter :: Ord Meter
instance showMeter :: Show Meter where
    show (Meter m) = "Meter(" <> show m <> ")"
instance encodeMeter :: Encode Meter where
    encode (Meter m) = encode m
instance decodeMeter :: Decode Meter where
    decode = map Meter <<< decode

meter :: Number -> Meter
meter = Meter

meterVal :: Meter -> Number
meterVal (Meter m) = m

inch :: Number -> Meter
inch i = meter $ i * 0.0254

feetInch :: Number -> Number -> Meter
feetInch f i = meter $ f * 0.3048 + i * 0.0254
