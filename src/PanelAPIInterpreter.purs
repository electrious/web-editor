module Editor.PanelAPIInterpreter where

import Prelude

import API (API, APIConfig, runAPI)
import API.Panel (createPanel, createPanels, deletePanels, deletePanelsInRoof, updatePanels)
import Control.Plus as Plus
import Data.Default (class Default, def)
import Data.Filterable (filter)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), foldl, fromFoldable, toUnfoldable, (:))
import Data.Map (delete, empty, insert, lookup, member, update, values)
import Data.Map.Internal (keys)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, emptyUUID)
import Editor.Common.Lenses (_apiConfig, _id, _leadId, _panels, _roof)
import Editor.PanelOperation (PanelOperation(..))
import Editor.Rendering.PanelRendering (_operations)
import FRP.Event (Event, fold, keepLatest, withLast)
import FRP.Event.Extra (debounce, leftmost, performEvent)
import Model.Roof.Panel (Panel, PanelDict, _uuid, isDifferent, panelDict)
import Model.Roof.RoofPlate (RoofPlate)

newtype PanelAPIInterpreterConfig = PanelAPIInterpreterConfig {
    apiConfig  :: APIConfig,
    roof       :: RoofPlate,
    clientId   :: UUID,
    panels     :: List Panel,
    operations :: Event PanelOperation
}

derive instance newtypePanelAPIInterpreterConfig :: Newtype PanelAPIInterpreterConfig _
instance defaultPanelAPIInterpreterConfig :: Default PanelAPIInterpreterConfig where
    def = PanelAPIInterpreterConfig {
        apiConfig  : def,
        roof       : def,
        clientId   : emptyUUID,
        panels     : Nil,
        operations : Plus.empty
    }

_clientId :: forall t a r. Newtype t { clientId :: a | r } => Lens' t a
_clientId = _Newtype <<< prop (SProxy :: SProxy "clientId")

newtype PanelAPIInterpreter = PanelAPIInterpreter {
    finished :: Event Unit
}

derive instance newtypePanelAPIInterpreter :: Newtype PanelAPIInterpreter _

_finished :: forall t a r. Newtype t { finished :: a | r } => Lens' t a
_finished = _Newtype <<< prop (SProxy :: SProxy "finished")


-- apply panel operations to internal panel dicts
applyOp :: PanelOperation -> PanelDict -> PanelDict
applyOp (AddPanel p) m      = insert (p ^. _uuid) p m
applyOp (AddPanels ps) m    = foldl (\m p -> insert (p ^. _uuid) p m) m ps
applyOp (DelPanel pid) m    = delete pid m
applyOp (DelPanels pids) m  = foldl (flip delete) m pids
applyOp DeleteAll m         = empty
applyOp (UpdatePanels ps) m = foldl (\m p -> update (const $ Just p) (p ^. _uuid) m) m ps


-- calculate diff between two PanelsDict and return the least needed PanelOperations
diff :: PanelDict -> PanelDict -> List PanelOperation
diff newD oldD = fromFoldable [AddPanels new, DelPanels del, UpdatePanels upd]
    where f (Tuple new upd) p = case lookup (p ^. _uuid) oldD of
                                    Just op -> if isDifferent p op
                                               then Tuple new (p : upd)
                                               else Tuple new upd
                                    Nothing -> Tuple (p : new) upd
          Tuple new upd = foldl f (Tuple Nil Nil) (values newD)
          del = filter (\i -> not $ member i newD) (keys oldD)


mkPanelAPIInterpreter :: PanelAPIInterpreterConfig -> PanelAPIInterpreter
mkPanelAPIInterpreter cfg = PanelAPIInterpreter { finished: debounce t apiResEvt }
    where roof   = cfg  ^. _roof
          leadId = roof ^. _leadId
          roofId = roof ^. _id
          apiCfg = cfg  ^. _apiConfig

          t = Milliseconds 2000.0

          dictState = panelDict $ cfg ^. _panels
          newStEvt  = fold applyOp (cfg ^. _operations) dictState
    
          calcOp { last: Just od, now: nd } = diff nd od
          calcOp { last: Nothing, now: _  } = Nil

          newOpEvt  = calcOp <$> withLast (debounce t newStEvt)

          runOps    = map leftmost <<< traverse (flip runAPI apiCfg <<< callPanelAPI leadId roofId)
          apiResEvt = keepLatest $ performEvent $ runOps <$> newOpEvt

callPanelAPI :: Int -> UUID -> PanelOperation -> API (Event Unit)
callPanelAPI leadId roofId (AddPanel p)      = createPanel leadId p
callPanelAPI leadId roofId (AddPanels ps)    = createPanels leadId roofId (toUnfoldable ps)
callPanelAPI leadId roofId (DelPanel pid)    = deletePanels leadId [pid]
callPanelAPI leadId roofId (DelPanels pids)  = deletePanels leadId (toUnfoldable pids)
callPanelAPI leadId roofId DeleteAll         = deletePanelsInRoof leadId roofId
callPanelAPI leadId roofId (UpdatePanels ps) = updatePanels leadId (toUnfoldable ps)
