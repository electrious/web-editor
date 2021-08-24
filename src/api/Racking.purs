module API.Racking where

import Prelude

import API (API, APIConfig, _baseUrl, callAPI', runAPI)
import Control.Plus (empty)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Lens (Lens', (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UUIDMap (toObject)
import Data.UUIDWrapper (UUID)
import Effect (Effect)
import FRP.Event (Event)
import Model.Racking.RackingSystem (RackingSystem)
import Model.Racking.RoofParameter (RoofParameter)
import Model.Roof.Panel (Panel)
import Type.Proxy (Proxy(..))

newtype PanelSize = PanelSize {
    long  :: Meter,
    short :: Meter
}

derive instance Newtype PanelSize _
derive instance Generic PanelSize _
instance Show PanelSize where
    show = genericShow
instance Default PanelSize where
    def = PanelSize { long : meter 1.6, short : meter 1.0 }
instance EncodeJson PanelSize where
    encodeJson (PanelSize p) = "l" := p.long
                            ~> "s" := p.short
                            ~> jsonEmptyObject

newtype RackRequest = RackRequest {
    parameters :: Map UUID RoofParameter,
    panels     :: Array Panel,
    size       :: PanelSize
}

derive instance Newtype RackRequest _
derive instance Generic RackRequest _
instance Show RackRequest where
    show = genericShow
instance Default RackRequest where
    def = RackRequest {
        parameters : empty,
        panels     : empty,
        size       : def
    }
instance EncodeJson RackRequest where
    encodeJson (RackRequest r) = "prm" := toObject r.parameters
                              ~> "ps" := r.panels
                              ~> "sz" := r.size
                              ~> jsonEmptyObject

_parameters :: forall t a r. Newtype t { parameters :: a | r } => Lens' t a
_parameters = _Newtype <<< prop (Proxy :: Proxy "parameters")

rackBaseUrl :: String
rackBaseUrl = "http://racking.staging.electrious.com"

doRack :: RackRequest -> API (Event RackingSystem)
doRack req = callAPI' POST "/racking" req


runRackAPI :: forall a. API a -> APIConfig -> Effect a
runRackAPI api cfg = runAPI api (cfg # _baseUrl .~ rackBaseUrl)
