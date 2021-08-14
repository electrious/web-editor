module Model.Racking.RoofParameter where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:?))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Array (foldl)
import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Model.Racking.BX.BXRoofParameter (BXRoofParameter)
import Model.Racking.FX.FXRoofParameter (FXRoofParameter)
import Model.Racking.GAF.GAFRoofParameter (GAFRoofParameter)
import Model.Racking.XR.XRRoofParameter (XRRoofParameter)
import Model.Racking.XRFlat.XRFlatRoofParameter (XRFlatRoofParameter)

data RoofParameter = XRParameter XRRoofParameter
                   | FXParameter FXRoofParameter
                   | XRFlatParameter XRFlatRoofParameter
                   | BXParameter BXRoofParameter
                   | GAFParameter GAFRoofParameter

derive instance Generic RoofParameter _
instance Show RoofParameter where
    show = genericShow
instance Default RoofParameter where
    def = XRParameter def
instance EncodeJson RoofParameter where
    encodeJson (XRParameter p)     = "xr"  := p ~> jsonEmptyObject
    encodeJson (FXParameter p)     = "fx"  := p ~> jsonEmptyObject
    encodeJson (XRFlatParameter p) = "fl"  := p ~> jsonEmptyObject
    encodeJson (BXParameter p)     = "bx"  := p ~> jsonEmptyObject
    encodeJson (GAFParameter p)    = "gaf" := p ~> jsonEmptyObject
instance DecodeJson RoofParameter where
    decodeJson = decodeJson >=> f
        where f o = do
                xr <- map XRParameter <$> o .:? "xr"
                fx <- map FXParameter <$> o .:? "fx"
                fl <- map XRFlatParameter <$> o .:? "fl"
                bx <- map BXParameter <$> o .:? "bx"
                gaf <- map GAFParameter <$> o .:? "gaf"

                pure $ candidate [xr, fx, fl, bx, gaf]

-- find a candidate value from a list of Maybe values, if all are Nothing, then
-- use a Default value.
candidate :: forall a. Default a => Array (Maybe a) -> a
candidate = fromMaybe def <<< foldl f Nothing
    where f a@(Just _) _ = a
          f Nothing b@(Just _) = b
          f Nothing Nothing  = Nothing
          