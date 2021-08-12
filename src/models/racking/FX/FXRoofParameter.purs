module Model.Racking.FX.FXRoofParameter where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Newtype (class Newtype)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)
import Model.Racking.MountSpacing (MountSpacing)
import Model.Racking.RafterSpacing (RafterSpacing)

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
