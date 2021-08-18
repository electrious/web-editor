module API.Racking where

import Prelude

import API (API, callAPI')
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Map (Map)
import Data.Meter (Meter)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UUIDWrapper (UUID)
import FRP.Event (Event)
import Model.Racking.RackingSystem (RackingSystem)
import Model.Racking.RoofParameter (RoofParameter)
import Model.Roof.Panel (Panel)

newtype PanelSize = PanelSize {
    long  :: Meter,
    short :: Meter
}

derive instance Newtype PanelSize _
derive instance Generic PanelSize _
instance Show PanelSize where
    show = genericShow
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
instance EncodeJson RackRequest where
    encodeJson (RackRequest r) = "prm" := r.parameters
                              ~> "ps" := r.panels
                              ~> "sz" := r.size
                              ~> jsonEmptyObject


rackBaseUrl :: String
rackBaseUrl = "http://racking.staging.electrious.com"

doRack :: RackRequest -> API (Event RackingSystem)
doRack req = callAPI' POST "/racking" req
