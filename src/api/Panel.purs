module API.Panel where

import Prelude

import API (API, callAPI')
import Axios (Method(..))
import Data.Lens ((^.))
import Data.UUID (UUID, toString)
import FRP.Event (Event)
import Model.Roof.Panel (Panel, _roofUUID)

createPanel :: Int -> Panel -> API (Event Unit)
createPanel leadId p = callAPI' POST url p
    where url = "/v1/leads/" <> show leadId <> "/roofplates/" <> show (p ^. _roofUUID) <> "/panels"

createPanels :: Int -> UUID -> Array Panel -> API (Event Unit)
createPanels leadId roofId ps = callAPI' POST url ps
    where url = "/v1/leads/" <> show leadId <> "/roofplates/" <> toString roofId <> "/panels"

deletePanels :: Int -> Array UUID -> API (Event Unit)
deletePanels leadId pids = callAPI' DELETE url pids
    where url = "/v1/leads/" <> show leadId <> "/panels"

deletePanelsInRoof :: Int -> UUID -> API (Event Unit)
deletePanelsInRoof leadId roofId = callAPI' DELETE url {}
    where url = "/v1/leads/" <> show leadId <> "/roofplates/" <> toString roofId <> "/panels"

updatePanels :: Int -> Array Panel -> API (Event Unit)
updatePanels leadId ps = callAPI' PUT url ps
    where url = "/v1/leads/" <> show leadId <> "/panels"
