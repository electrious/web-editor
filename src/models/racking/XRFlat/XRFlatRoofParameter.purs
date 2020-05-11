module Model.Racking.XRFlat.XRFlatRoofParameter where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)

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
