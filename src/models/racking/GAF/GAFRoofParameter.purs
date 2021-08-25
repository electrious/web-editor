module Model.Racking.GAF.GAFRoofParameter where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

newtype GAFRoofParameter = GAFRoofParameter {}

derive instance Newtype GAFRoofParameter _
derive instance Generic GAFRoofParameter _
instance Show GAFRoofParameter where
    show = genericShow
instance EncodeJson GAFRoofParameter where
    encodeJson _ = jsonEmptyObject
instance DecodeJson GAFRoofParameter where
    decodeJson = const $ pure $ GAFRoofParameter {}
