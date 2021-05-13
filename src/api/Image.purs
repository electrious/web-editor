module API.Image where

import API (API, callAPI')
import Axios.Types (Method(..))
import Data.Default (class Default)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.Show (class Show)
import FRP.Event (Event)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)

-- image api request data
newtype ImageReq = ImageReq {
    leadId   :: Int,
    provider :: String
    }

derive instance genericImageReq :: Generic ImageReq _
derive instance newtypeImageReq :: Newtype ImageReq _
instance showImageReq :: Show ImageReq where
    show = genericShow
instance defaultImageReq :: Default ImageReq where
    def = ImageReq { leadId : 0, provider : "google-maps" }
instance encodeImageReq :: Encode ImageReq where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true,
                                             fieldTransform = toJSFieldName
                                           })

toJSFieldName :: String -> String
toJSFieldName "leadId"   = "ID"
toJSFieldName "provider" = "Provider"
toJSFieldName _          = ""

-- Image api response data
newtype ImageResp = ImageResp {
    link          :: String,
    pixelPerMeter :: Int,
    provider      :: String
    }
derive instance genericImageResp :: Generic ImageResp _
derive instance newtypeImageResp :: Newtype ImageResp _
instance showImageResp :: Show ImageResp where
    show = genericShow
instance decodeImageResp :: Decode ImageResp where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true,
                                             fieldTransform = toRespJSField })

toRespJSField :: String -> String
toRespJSField "link"          = "Link"
toRespJSField "pixelPerMeter" = "PixelsPerMeter"
toRespJSField "provider"      = "Provider"
toRespJSField _               = ""


getImageMeta :: ImageReq -> API (Event ImageResp)
getImageMeta req = callAPI' POST "/v3/projects.GetHouseImage" req
