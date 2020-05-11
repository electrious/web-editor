module Model.Racking.GAF.GAFRoofParameter where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)

newtype GAFRoofParameter = GAFRoofParameter {}

derive instance newtypeGAFRoofParameter :: Newtype GAFRoofParameter _
derive instance genericGAFRoofParameter :: Generic GAFRoofParameter _
instance showGAFRoofParameter :: Show GAFRoofParameter where
    show = genericShow
instance encodeGAFRoofParameter :: Encode GAFRoofParameter where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
instance decodeGAFRoofParameter :: Decode GAFRoofParameter where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })
