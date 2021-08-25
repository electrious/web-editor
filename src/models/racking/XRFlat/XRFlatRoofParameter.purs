module Model.Racking.XRFlat.XRFlatRoofParameter where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:?))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

newtype XRFlatRoofParameter = XRFlatRoofParameter {
    rowGap :: Maybe Int
}

derive instance Newtype XRFlatRoofParameter _
derive instance Generic XRFlatRoofParameter _
instance Show XRFlatRoofParameter where
    show = genericShow
instance EncodeJson XRFlatRoofParameter where
    encodeJson (XRFlatRoofParameter p) = "rowGap" := p.rowGap
                                      ~> jsonEmptyObject
instance DecodeJson XRFlatRoofParameter where
    decodeJson = decodeJson >=> f 
        where f o = mkXRFlatRoofParameter <$> o .:? "rowGap"

mkXRFlatRoofParameter :: Maybe Int -> XRFlatRoofParameter
mkXRFlatRoofParameter g = XRFlatRoofParameter { rowGap : g }
