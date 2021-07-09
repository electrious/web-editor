module API.Image where

import API (API, callAPI')
import Axios.Types (Method(..))
import Control.Category ((<<<))
import Data.Default (class Default)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Show (class Show)
import Data.Symbol (SProxy(..))
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

_provider :: forall t a r. Newtype t { provider :: a | r } => Lens' t a
_provider = _Newtype <<< prop (SProxy :: SProxy "provider")


-- Image api response data
newtype ImageResp = ImageResp {
    link          :: String,
    pixelPerMeter :: Number,
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

_link :: forall t a r. Newtype t { link :: a | r } => Lens' t a
_link = _Newtype <<< prop (SProxy :: SProxy "link")

_pixelPerMeter :: forall t a r. Newtype t { pixelPerMeter :: a | r } => Lens' t a
_pixelPerMeter = _Newtype <<< prop (SProxy :: SProxy "pixelPerMeter")

getImageMeta :: ImageReq -> API (Event ImageResp)
getImageMeta req = callAPI' POST "/v3/projects.GetHouseImage" req
