module Model.Racking.GAF.GAFRoofParameter where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Newtype (class Newtype)
import Editor.Common.ProtoCodable (class ProtoDecodable, class ProtoEncodable)
import Effect (Effect)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)

foreign import data GAFParameterPB :: Type
foreign import mkGAFParameterPB :: Effect GAFParameterPB

newtype GAFRoofParameter = GAFRoofParameter {}

derive instance newtypeGAFRoofParameter :: Newtype GAFRoofParameter _
derive instance genericGAFRoofParameter :: Generic GAFRoofParameter _
instance showGAFRoofParameter :: Show GAFRoofParameter where
    show = genericShow
instance encodeGAFRoofParameter :: Encode GAFRoofParameter where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
instance decodeGAFRoofParameter :: Decode GAFRoofParameter where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })
instance protoEncodableGAFRoofParameter :: ProtoEncodable GAFRoofParameter GAFParameterPB where
    toProto _ = mkGAFParameterPB
instance protoDecodableGAFRoofParameter :: ProtoDecodable GAFRoofParameter GAFParameterPB where
    fromProto _ = GAFRoofParameter {}
