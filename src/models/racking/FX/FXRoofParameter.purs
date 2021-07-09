module Model.Racking.FX.FXRoofParameter where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens ((^.))
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_mountSpacing, _rafterSpacing)
import Editor.Common.ProtoCodable (class ProtoDecodable, class ProtoEncodable, fromProto, toProto)
import Effect (Effect)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)
import Model.Racking.Class (class HasSpacings, getMountSpacing, getRafterSpacing, setMountSpacing, setRafterSpacing)
import Model.Racking.MountSpacing (MountSpacing)
import Model.Racking.RafterSpacing (RafterSpacing)

foreign import data FXParameterPB :: Type
foreign import mkFXParameterPB :: Effect FXParameterPB
instance hasSpacingsFXParameterPB :: HasSpacings FXParameterPB

newtype FXRoofParameter = FXRoofParameter {
    mountSpacing  :: MountSpacing,
    rafterSpacing :: RafterSpacing
}

toJSFieldName :: String -> String
toJSFieldName "mountSpacing"  = "mount_space"
toJSFieldName "rafterSpacing" = "rafter_space"
toJSFieldName _               = "mount_space"


derive instance newtypeFXRoofParameter :: Newtype FXRoofParameter _
derive instance genericFXRoofParameter :: Generic FXRoofParameter _
instance showFXRoofParameter :: Show FXRoofParameter where
    show = genericShow
instance encodeFXRoofParameter :: Encode FXRoofParameter where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true,
                                             fieldTransform = toJSFieldName
                                            })
instance decodeFXRoofParameter :: Decode FXRoofParameter where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true,
                                             fieldTransform = toJSFieldName
                                            })
instance protoEncodableFXRoofParameter :: ProtoEncodable FXRoofParameter FXParameterPB where
    toProto p = do
        fp <- mkFXParameterPB
        ms <- toProto $ p ^. _mountSpacing
        setMountSpacing ms fp
        rs <- toProto $ p ^. _rafterSpacing
        setRafterSpacing rs fp
        pure fp
instance protoDecodableFXRoofParameter :: ProtoDecodable FXRoofParameter FXParameterPB where
    fromProto p = FXRoofParameter {
        mountSpacing  : fromProto $ getMountSpacing p,
        rafterSpacing : fromProto $ getRafterSpacing p
    }