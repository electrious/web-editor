module API.Image where

import Prelude
import API (API, callAPI')
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Default (class Default)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import FRP.Event (Event)
import Type.Proxy (Proxy(..))

-- image api request data
newtype ImageReq = ImageReq {
    leadId   :: Int,
    provider :: String
    }

derive instance Generic ImageReq _
derive instance Newtype ImageReq _
instance Show ImageReq where
    show = genericShow
instance Default ImageReq where
    def = ImageReq { leadId : 0, provider : "google-maps" }
instance EncodeJson ImageReq where
    encodeJson (ImageReq r) = "ID" := r.leadId
                           ~> "Provider" := r.provider
                           ~> jsonEmptyObject

_provider :: forall t a r. Newtype t { provider :: a | r } => Lens' t a
_provider = _Newtype <<< prop (Proxy :: Proxy "provider")


-- Image api response data
newtype ImageResp = ImageResp {
    link          :: String,
    pixelPerMeter :: Number,
    provider      :: String
    }
derive instance Generic ImageResp _
derive instance Newtype ImageResp _
instance Show ImageResp where
    show = genericShow
instance DecodeJson ImageResp where
    decodeJson = decodeJson >=> f
        where f o = mkImageResp <$> o .: "Link"
                                <*> o .: "PixelPerMeter"
                                <*> o .: "Provider"

mkImageResp :: String -> Number -> String -> ImageResp
mkImageResp link pixelPerMeter provider = ImageResp { link, pixelPerMeter, provider }

_link :: forall t a r. Newtype t { link :: a | r } => Lens' t a
_link = _Newtype <<< prop (Proxy :: Proxy "link")

_pixelPerMeter :: forall t a r. Newtype t { pixelPerMeter :: a | r } => Lens' t a
_pixelPerMeter = _Newtype <<< prop (Proxy :: Proxy "pixelPerMeter")

getImageMeta :: ImageReq -> API (Event ImageResp)
getImageMeta req = callAPI' POST "/v3/projects.GetHouseImage" req
