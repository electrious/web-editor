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
