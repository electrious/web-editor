module Model.Racking.XR.XRRoofParameter where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((^.))
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_mountSpacing, _rafterSpacing)
import Editor.Common.ProtoCodable (class ProtoDecodable, class ProtoEncodable, fromProto, toProto)
import Effect (Effect)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)
import Model.Racking.Class (class HasSpacings, getMountSpacing, getRafterSpacing, setMountSpacing, setRafterSpacing)
import Model.Racking.MountSpacing (MountSpacing)
import Model.Racking.RafterSpacing (RafterSpacing)

foreign import data XRParameterPB :: Type
foreign import mkXRParameterPB :: Effect XRParameterPB

instance hasSpacingsXRParameterPB :: HasSpacings XRParameterPB

newtype XRRoofParameter = XRRoofParameter {
    mountSpacing  :: MountSpacing,
    rafterSpacing :: RafterSpacing
}

toJSFieldName :: String -> String
toJSFieldName "mountSpacing"  = "mount_space"
toJSFieldName "rafterSpacing" = "rafter_space"
toJSFieldName _               = "mount_space"

derive instance newtypeXRRoofParameter :: Newtype XRRoofParameter _
derive instance genericXRRoofParameter :: Generic XRRoofParameter _
instance showXRRoofParameter :: Show XRRoofParameter where
    show = genericShow
instance encodeXRRoofParameter :: Encode XRRoofParameter where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true,
                                             fieldTransform = toJSFieldName
                                            })
instance decodeXRRoofParameter :: Decode XRRoofParameter where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true,
                                             fieldTransform = toJSFieldName
                                            })
instance protoEncodableXRRoofParameter :: ProtoEncodable XRRoofParameter XRParameterPB where
    toProto p = do
        xp <- mkXRParameterPB
        ms <- toProto $ p ^. _mountSpacing
        setMountSpacing ms xp
        rs <- toProto $ p ^. _rafterSpacing
        setRafterSpacing rs xp
        pure xp
instance protoDecodableXRRoofParameter :: ProtoDecodable XRRoofParameter XRParameterPB where
    fromProto p = XRRoofParameter {
        mountSpacing  : fromProto $ getMountSpacing p,
        rafterSpacing : fromProto $ getRafterSpacing p
    }