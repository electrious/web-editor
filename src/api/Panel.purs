module API.Panel where

import Prelude

import API (API, callAPI')
import Axios.Types (Method(..))
import Data.Lens ((^.))
import Data.UUID (UUID, toString)
import Editor.Common.Lenses (_alignment, _orientation, _slope, _x, _y)
import FRP.Event (Event)
import Model.Roof.Panel (Panel, _arrNumber, _roofUUID, _row_number, _uuid)


-- delete id field in Panel to be used in create panel APIs
delPanelId p = {
    uuid           : p ^. _uuid,
    roofplate_uuid : p ^. _roofUUID,
    row_number     : p ^. _row_number,
    array_number   : p ^. _arrNumber,
    x              : p ^. _x,
    y              : p ^. _y,
    slope          : p ^. _slope,
    orientation    : p ^. _orientation,
    alignment      : p ^. _alignment
}

createPanel :: Int -> Panel -> API (Event Unit)
createPanel leadId p = callAPI' POST url $ delPanelId p
    where url = "/leads/" <> show leadId <> "/roofplates/" <> show (p ^. _roofUUID) <> "/panels"

createPanels :: Int -> UUID -> Array Panel -> API (Event Unit)
createPanels leadId roofId ps = callAPI' POST url $ delPanelId <$> ps
    where url = "/leads/" <> show leadId <> "/roofplates/" <> toString roofId <> "/panels"

deletePanels :: Int -> Array UUID -> API (Event Unit)
deletePanels leadId pids = callAPI' DELETE url { uuids: pids }
    where url = "/leads/" <> show leadId <> "/panels"

deletePanelsInRoof :: Int -> UUID -> API (Event Unit)
deletePanelsInRoof leadId roofId = callAPI' DELETE url {}
    where url = "/leads/" <> show leadId <> "/roofplates/" <> toString roofId <> "/panels"

updatePanels :: Int -> Array Panel -> API (Event Unit)
updatePanels leadId ps = callAPI' PUT url ps
    where url = "/leads/" <> show leadId <> "/panels"
