module Model.Racking.BX.BXRoofParameter where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Editor.Common.ProtoCodable (class ProtoDecodable, class ProtoEncodable, fromProto, toProto)
import Effect (Effect)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)
import Model.Racking.BX.Chassis (ChassisKind, ChassisType)

foreign import data BXParameterPB :: Type
foreign import mkBXParameterPB :: Effect BXParameterPB

foreign import getChassisKind :: BXParameterPB -> ChassisKind
foreign import setChassisKind :: ChassisKind -> BXParameterPB -> Effect Unit

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
instance protoEncodableBXRoofParameter :: ProtoEncodable BXRoofParameter BXParameterPB where
    toProto (BXRoofParameter { chassisType }) = do
        bp <- mkBXParameterPB
        ct <- toProto chassisType
        setChassisKind ct bp
        pure bp
instance protoDecodableBXRoofParameter :: ProtoDecodable BXRoofParameter BXParameterPB where
    fromProto p = BXRoofParameter {
        chassisType: fromProto $ getChassisKind p
    }