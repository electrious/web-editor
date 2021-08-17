module API.Panel where

import Prelude

import API (API, callAPI')
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Lens ((^.))
import Data.UUIDWrapper (UUID, toString)
import FRP.Event (Event)
import Model.Roof.Panel (Panel, _roofUUID)


newtype PanelsResult = PanelsResult {
    panels :: Array Panel
}

derive instance Generic PanelsResult _
instance DecodeJson PanelsResult where
    decodeJson = decodeJson >=> f
        where f o = mkPanelsResult <$> o .: "panels"

mkPanelsResult :: Array Panel -> PanelsResult
mkPanelsResult panels = PanelsResult { panels }

loadPanels :: Int -> API (Event (Array Panel))
loadPanels i = map f <$> callAPI' GET url {}
    where url = "/v1/leads/" <> show i <> "/panels"
          f (PanelsResult r) = r.panels

createPanel :: Int -> Panel -> API (Event Unit)
createPanel leadId p = callAPI' POST url p
    where url = "/v1/leads/" <> show leadId <> "/roofplates/" <> show (p ^. _roofUUID) <> "/panels"

createPanels :: Int -> UUID -> Array Panel -> API (Event Unit)
createPanels leadId roofId ps = callAPI' POST url ps
    where url = "/v1/leads/" <> show leadId <> "/roofplates/" <> toString roofId <> "/panels"

deletePanels :: Int -> Array UUID -> API (Event Unit)
deletePanels leadId pids = callAPI' DELETE url { uuids: pids }
    where url = "/v1/leads/" <> show leadId <> "/panels"

deletePanelsInRoof :: Int -> UUID -> API (Event Unit)
deletePanelsInRoof leadId roofId = callAPI' DELETE url {}
    where url = "/v1/leads/" <> show leadId <> "/roofplates/" <> toString roofId <> "/panels"

updatePanels :: Int -> Array Panel -> API (Event Unit)
updatePanels leadId ps = callAPI' PUT url ps
    where url = "/v1/leads/" <> show leadId <> "/panels"
