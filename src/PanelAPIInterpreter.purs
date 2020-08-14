module Editor.PanelAPIInterpreter where

import Prelude

import API (APIConfig)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List, foldl)
import Data.Map (delete, empty, insert, update)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.UUID (UUID)
import Editor.PanelOperation (PanelOperation(..))
import Editor.RoofManager (PanelsDict)
import FRP.Event (Event)
import Model.Roof.Panel (Panel, _uuid)
import Model.Roof.RoofPlate (RoofPlate)

newtype PanelAPIInterpreterConfig = PanelAPIInterpreterConfig {
    apiConfig  :: APIConfig,
    roof       :: RoofPlate,
    clientId   :: UUID,
    panels     :: List Panel,
    operations :: Event PanelOperation
}

derive instance newtypePanelAPIInterpreterConfig :: Newtype PanelAPIInterpreterConfig _

_clientId :: forall t a r. Newtype t { clientId :: a | r } => Lens' t a
_clientId = _Newtype <<< prop (SProxy :: SProxy "clientId")

newtype PanelAPIInterpreter = PanelAPIInterpreter {
    finished :: Event Unit
}

derive instance newtypePanelAPIInterpreter :: Newtype PanelAPIInterpreter _

_finished :: forall t a r. Newtype t { finished :: a | r } => Lens' t a
_finished = _Newtype <<< prop (SProxy :: SProxy "finished")


-- apply panel operations to internal panel dicts
applyOp :: PanelOperation -> PanelsDict -> PanelsDict
applyOp (AddPanel p) m      = insert (p ^. _uuid) p m
applyOp (AddPanels ps) m    = traverse (\p -> insert (p ^. _uuid) p m) ps
applyOp (DelPanel pid) m    = delete pid m
applyOp (DelPanels pids) m  = foldl (flip delete) m pids
applyOp DeleteAll m         = empty
applyOp (UpdatePanels ps) m = foldl (\p -> update (const p) (p ^. _uuid)) m ps
