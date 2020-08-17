module Editor.PanelAPIInterpreter where

import Prelude

import API (APIConfig)
import API.Panel (createPanel, createPanels, deletePanels, deletePanelsInRoof, updatePanels)
import Data.Filterable (filter)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), foldl, fromFoldable)
import Data.List.Lazy (concat)
import Data.Map (delete, empty, insert, lookup, member, update, values)
import Data.Map.Internal (keys)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.UUID (UUID)
import Editor.Common.Lenses (_apiConfig, _id, _leadId, _panels)
import Editor.PanelOperation (PanelOperation(..))
import Editor.Rendering.PanelRendering (_operations)
import Editor.RoofManager (PanelsDict, panelDict)
import Effect (Effect)
import FRP.Event (Event, fold, keepLatest, withLast)
import FRP.Event.Extra (debounce, performEvent)
import Model.Roof.Panel (Panel, _uuid, isDifferent)
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


-- calculate diff between two PanelsDict and return the least needed PanelOperations
diff :: PanelsDict -> PanelsDict -> List PanelOperation
diff newD oldD = concat $ fromFoldable [AddPanels <$> new, DelPanels <$> del, UpdatePanels <$> upd]
    where f (Tuple new upd) p = case lookup (p ^. _uuid) oldD of
                                    Just op -> if isDifferent p op
                                               then Tuple new (p : upd)
                                               else Tuple new upd
                                    Nothing -> Tuple (p : new) upd
          Tuple new upd = foldl f (Tuple Nil Nil) (values newD)
          del = filter (not <<< member _ newD) (keys oldD)


mkPanelAPIInterpreter :: PanelAPIInterpreterConfig -> PanelAPIInterpreter
mkPanelAPIInterpreter cfg = PanelAPIInterpreter { finished: debounce t apiResEvt }
    where roof   = cfg  ^. _roof
          leadId = roof ^. _leadId
          roofId = roof ^. _id
          apiCfg = cfg  ^. _apiConfig

          t = Milliseconds 2000

          dictState = panelDict $ cfg ^. _panels
          newStEvt  = fold applyOp (cfg ^. _operations) dictState
    
          calcOp { last: Just od, now: nd } = diff nd od
          calcOp { last: Nothing, now: _  } = Nil

          newOpEvt  = calcOp <$> withLast (debounce t newOps)

          apiResEvt = keepLatest $ performEvent $ flip runAPI apiCfg <<< callPanelAPI leadId roofId <$> newOpEvt

callPanelAPI :: Int -> UUID -> PanelOperation -> API (Event Unit)
callPanelAPI leadId roofId (AddPanel p)      = createPanel leadId p
callPanelAPI leadId roofId (AddPanels ps)    = createPanels leadId roofId ps
callPanelAPI leadId roofId (DelPanel pid)    = deletePanels leadId [pid]
callPanelAPI leadId roofId (DelPanels pids)  = deletePanels leadId (fromFoldable pids)
callPanelAPI leadId roofId DeleteAll         = deletePanelsInRoof leadId roofId
callPanelAPI leadId roofId (UpdatePanels ps) = updatePanels leadId ps
