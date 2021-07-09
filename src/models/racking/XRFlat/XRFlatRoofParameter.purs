module Model.Racking.XRFlat.XRFlatRoofParameter where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Editor.Common.ProtoCodable (class ProtoDecodable, class ProtoEncodable)
import Effect (Effect)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)

foreign import data XRFlatParameterPB :: Type
foreign import mkXRFlatParameterPB :: Effect XRFlatParameterPB

newtype XRFlatRoofParameter = XRFlatRoofParameter {
    rowGap :: Maybe Int
}

derive instance newtypeXRFlatRoofParameter :: Newtype XRFlatRoofParameter _
derive instance genericXRFlatRoofParameter :: Generic XRFlatRoofParameter _
instance showXRFlatRoofParameter :: Show XRFlatRoofParameter where
    show = genericShow
instance encodeXRFlatRoofParameter :: Encode XRFlatRoofParameter where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
instance docodeXRFlatRoofParameter :: Decode XRFlatRoofParameter where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })
instance protoEncodableXRFlatRoofParameter :: ProtoEncodable XRFlatRoofParameter XRFlatParameterPB where
    toProto _ = mkXRFlatParameterPB
instance protoDecodableXRFlatRoofParameter :: ProtoDecodable XRFlatRoofParameter XRFlatParameterPB where
    fromProto _ = XRFlatRoofParameter { rowGap : Nothing }
