module Editor.PanelAPIInterpreter where

import Prelude

import API (API, APIConfig, runAPI)
import API.Panel (createPanel, createPanels, deletePanels, deletePanelsInRoof, updatePanels)
import Control.Plus as Plus
import Data.Default (class Default, def)
import Data.Filterable (filter)
import Data.Foldable (null)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), foldl, fromFoldable, toUnfoldable, (:))
import Data.Map (delete, empty, insert, lookup, member, update, values)
import Data.Map as Map
import Data.Map.Internal (keys)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (class Foldable, traverse)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, emptyUUID)
import Editor.Common.Lenses (_apiConfig, _id, _leadId, _roof)
import Editor.PanelOperation (PanelOperation(..))
import Editor.Rendering.PanelRendering (_operations)
import FRP.Event (Event, fold, keepLatest, withLast)
import FRP.Event.Extra (anyEvt, debounce, performEvent)
import Model.Roof.Panel (PanelDict, _uuid, isDifferent)
import Model.Roof.RoofPlate (RoofPlate)

newtype PanelAPIInterpreterConfig = PanelAPIInterpreterConfig {
    apiConfig  :: APIConfig,
    roof       :: RoofPlate,
    clientId   :: UUID,
    operations :: Event PanelOperation
}

derive instance newtypePanelAPIInterpreterConfig :: Newtype PanelAPIInterpreterConfig _
instance defaultPanelAPIInterpreterConfig :: Default PanelAPIInterpreterConfig where
    def = PanelAPIInterpreterConfig {
        apiConfig  : def,
        roof       : def,
        clientId   : emptyUUID,
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
applyOp (LoadPanels ps) m   = foldl (\m p -> insert (p ^. _uuid) p m) m ps
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

          newStEvt  = fold applyOp (cfg ^. _operations) Map.empty
    
          calcOp { last: Just od, now: nd } = diff nd od
          calcOp { last: Nothing, now: _  } = Nil

          newOpEvt  = calcOp <$> withLast (debounce t newStEvt)

          runOps    = map anyEvt <<< traverse (flip runAPI apiCfg <<< callPanelAPI leadId roofId)
          apiResEvt = keepLatest $ performEvent $ runOps <$> newOpEvt

callPanelAPI :: Int -> UUID -> PanelOperation -> API (Event Unit)
callPanelAPI leadId roofId (LoadPanels ps)   = pure Plus.empty
callPanelAPI leadId roofId (AddPanel p)      = createPanel leadId p
callPanelAPI leadId roofId (AddPanels ps)    = onlyCallFull ps (toUnfoldable >>> createPanels leadId roofId)
callPanelAPI leadId roofId (DelPanel pid)    = deletePanels leadId [pid]
callPanelAPI leadId roofId (DelPanels pids)  = onlyCallFull pids (toUnfoldable >>> deletePanels leadId)
callPanelAPI leadId roofId DeleteAll         = deletePanelsInRoof leadId roofId
callPanelAPI leadId roofId (UpdatePanels ps) = onlyCallFull ps (toUnfoldable >>> updatePanels leadId)

-- only call the provided API function if the param is not null
onlyCallFull :: forall f a. Foldable f => f a -> (f a -> API (Event Unit)) -> API (Event Unit)
onlyCallFull ls f = if null ls
                    then pure (pure unit)
                    else f ls