module Model.Racking.RoofParameter where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Editor.Common.ProtoCodable (class ProtoDecodable, class ProtoEncodable, fromProto, toProto)
import Effect (Effect)
import Foreign.Generic (class Decode, class Encode, Foreign, SumEncoding(..), defaultOptions, encode, genericDecode)
import Model.Racking.BX.BXRoofParameter (BXParameterPB, BXRoofParameter)
import Model.Racking.FX.FXRoofParameter (FXParameterPB, FXRoofParameter)
import Model.Racking.GAF.GAFRoofParameter (GAFParameterPB, GAFRoofParameter)
import Model.Racking.XR.XRRoofParameter (XRParameterPB, XRRoofParameter)
import Model.Racking.XRFlat.XRFlatRoofParameter (XRFlatParameterPB, XRFlatRoofParameter)

foreign import data RoofParameterPB :: Type
foreign import mkRoofParameterPB :: Effect RoofParameterPB

newtype ParamTypeCasePB = ParamTypeCasePB Int
derive newtype instance eqParamTypeCasePB :: Eq ParamTypeCasePB
foreign import paramTypeNotSet :: ParamTypeCasePB
foreign import paramTypeXR     :: ParamTypeCasePB
foreign import paramTypeFX     :: ParamTypeCasePB
foreign import paramTypeXRFlat :: ParamTypeCasePB
foreign import paramTypeBX     :: ParamTypeCasePB
foreign import paramTypeGAF    :: ParamTypeCasePB

foreign import getParamTypeCase :: RoofParameterPB -> ParamTypeCasePB
foreign import getXRParameter :: RoofParameterPB -> XRParameterPB
foreign import setXRParameter :: XRParameterPB -> RoofParameterPB -> Effect Unit
foreign import getFXParameter :: RoofParameterPB -> FXParameterPB
foreign import setFXParameter :: FXParameterPB -> RoofParameterPB -> Effect Unit
foreign import getXRFlatParameter :: RoofParameterPB -> XRFlatParameterPB
foreign import setXRFlatParameter :: XRFlatParameterPB -> RoofParameterPB -> Effect Unit
foreign import getBXParameter :: RoofParameterPB -> BXParameterPB
foreign import setBXParameter :: BXParameterPB -> RoofParameterPB -> Effect Unit
foreign import getGAFParameter :: RoofParameterPB -> GAFParameterPB
foreign import setGAFParameter :: GAFParameterPB -> RoofParameterPB -> Effect Unit

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
instance protoEncodableRoofParameter :: ProtoEncodable RoofParameter RoofParameterPB where
    toProto p = do
        rp <- mkRoofParameterPB
        case p of
            XRParameter xr -> do
                xrp <- toProto xr
                setXRParameter xrp rp
            FXParameter fx -> do
                fxp <- toProto fx
                setFXParameter fxp rp
            XRFlatParameter xf -> do
                xfp <- toProto xf
                setXRFlatParameter xfp rp
            BXParameter bx -> do
                bxp <- toProto bx
                setBXParameter bxp rp
            GAFParameter gaf -> do
                gafp <- toProto gaf
                setGAFParameter gafp rp
        pure rp
instance protoDecodableRoofParameter :: ProtoDecodable RoofParameter RoofParameterPB where
    fromProto r = f $ getParamTypeCase r
        where f v | v == paramTypeXR     = XRParameter $ fromProto $ getXRParameter r
                  | v == paramTypeFX     = FXParameter $ fromProto $ getFXParameter r
                  | v == paramTypeXRFlat = XRFlatParameter $ fromProto $ getXRFlatParameter r
                  | v == paramTypeBX     = BXParameter $ fromProto $ getBXParameter r
                  | v == paramTypeGAF    = GAFParameter $ fromProto $ getGAFParameter r
                  | otherwise            = XRParameter $ fromProto $ getXRParameter r
toParamTag :: String -> String
toParamTag "XRParameter"     = "xr"
toParamTag "FXParameter"     = "fx"
toParamTag "XRFlatParameter" = "fl"
toParamTag "BXParameter"     = "bx"
toParamTag "GAFParameter"    = "gaf"
toParamTag _                 = "xr"
