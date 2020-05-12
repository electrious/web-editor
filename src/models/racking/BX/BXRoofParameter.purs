module Model.Racking.BX.BXRoofParameter where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)
import Model.Racking.BX.Chassis (ChassisType)

newtype BXRoofParameter = BXRoofParameter {
    chassisType :: ChassisType
}

derive instance newtypeBXRoofParameter :: Newtype BXRoofParameter _
derive instance genericBXRoofParameter :: Generic BXRoofParameter _
instance showBXRoofParameter :: Show BXRoofParameter where
    show = genericShow
instance encodeBXRoofParameter :: Encode BXRoofParameter where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true,
                                             fieldTransform = const "chassis_type"
                                           })
instance decodeBXRoofParameter :: Decode BXRoofParameter where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true,
                                              fieldTransform = const "chassis_type"
                                            })