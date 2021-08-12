module Model.Racking.XR.XRRoofParameter where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Newtype (class Newtype)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)
import Model.Racking.MountSpacing (MountSpacing)
import Model.Racking.RafterSpacing (RafterSpacing)


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
