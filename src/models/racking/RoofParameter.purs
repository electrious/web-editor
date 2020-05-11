module Model.Racking.RoofParameter where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Foreign.Generic (class Decode, class Encode, Foreign, SumEncoding(..), defaultOptions, encode, genericDecode)
import Model.Racking.BX.BXRoofParameter (BXRoofParameter)
import Model.Racking.FX.FXRoofParameter (FXRoofParameter)
import Model.Racking.GAF.GAFRoofParameter (GAFRoofParameter)
import Model.Racking.XR.XRRoofParameter (XRRoofParameter)
import Model.Racking.XRFlat.XRFlatRoofParameter (XRFlatRoofParameter)

foreign import toTagged :: Foreign -> Foreign

data RoofParameter = XRParameter XRRoofParameter
                   | FXParameter FXRoofParameter
                   | XRFlatParameter XRFlatRoofParameter
                   | BXParameter BXRoofParameter
                   | GAFParameter GAFRoofParameter

derive instance genericRoofParameter :: Generic RoofParameter _
instance showRoofParameter :: Show RoofParameter where
    show = genericShow
instance encodeRoofParameter :: Encode RoofParameter where
    encode (XRParameter xr)       = encode { xr  : encode xr }
    encode (FXParameter fx)       = encode { fx  : encode fx }
    encode (XRFlatParameter flat) = encode { fl  : encode flat }
    encode (BXParameter bx)       = encode { bx  : encode bx }
    encode (GAFParameter gaf)     = encode { gaf : encode gaf }
instance decodeRoofParameter :: Decode RoofParameter where
    decode = toTagged >>> genericDecode (defaultOptions { sumEncoding = TaggedObject {
                                                                            tagFieldName: "tag",
                                                                            contentsFieldName: "contents",
                                                                            constructorTagTransform: toParamTag
                                                                        }
                                                        })

toParamTag :: String -> String
toParamTag "xr"  = "XRParameter"
toParamTag "fx"  = "FXParameter"
toParamTag "fl"  = "XRFlatParameter"
toParamTag "bx"  = "BXParameter"
toParamTag "gaf" = "GAFParameter"
toParamTag _     = "GAFParameter"
