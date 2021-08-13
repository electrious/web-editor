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
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (class Foldable, traverse)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, emptyUUID)
import Editor.Common.Lenses (_apiConfig, _id, _houseId, _roof)
import Editor.PanelOperation (PanelOperation(..))
import Editor.Rendering.PanelRendering (_operations)
import FRP.Event (Event, fold, keepLatest, withLast)
import FRP.Event.Extra (anyEvt, debounce, multicast, performEvent)
import Model.Roof.Panel (PanelDict, _uuid, isDifferent)
import Model.Roof.RoofPlate (RoofPlate)
import Type.Proxy (Proxy(..))

newtype PanelAPIInterpreterConfig = PanelAPIInterpreterConfig {
    apiConfig  :: APIConfig,
    roof       :: RoofPlate,
    houseId    :: Int,
    clientId   :: UUID,
    operations :: Event PanelOperation
}

derive instance newtypePanelAPIInterpreterConfig :: Newtype PanelAPIInterpreterConfig _
instance defaultPanelAPIInterpreterConfig :: Default PanelAPIInterpreterConfig where
    def = PanelAPIInterpreterConfig {
        apiConfig  : def,
        roof       : def,
        houseId    : 0,
        clientId   : emptyUUID,
        operations : Plus.empty
    }

_clientId :: forall t a r. Newtype t { clientId :: a | r } => Lens' t a
_clientId = _Newtype <<< prop (Proxy :: Proxy "clientId")

newtype PanelAPIInterpreter = PanelAPIInterpreter {
    finished :: Event Unit
}

derive instance newtypePanelAPIInterpreter :: Newtype PanelAPIInterpreter _

_finished :: forall t a r. Newtype t { finished :: a | r } => Lens' t a
_finished = _Newtype <<< prop (Proxy :: Proxy "finished")


-- apply panel operations to internal panel dicts
applyOp :: PanelOperation -> PanelDict -> PanelDict
applyOp (LoadPanels ps) m   = foldl (\d p -> insert (p ^. _uuid) p d) m ps
applyOp (AddPanel p) m      = insert (p ^. _uuid) p m
applyOp (AddPanels ps) m    = foldl (\d p -> insert (p ^. _uuid) p d) m ps
applyOp (DelPanel pid) m    = delete pid m
applyOp (DelPanels pids) m  = foldl (flip delete) m pids
applyOp DeleteAll _         = empty
applyOp (UpdatePanels ps) m = foldl (\d p -> update (const $ Just p) (p ^. _uuid) d) m ps

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
    where roof    = cfg  ^. _roof
          houseId = cfg  ^. _houseId
          roofId  = roof ^. _id
          apiCfg  = cfg  ^. _apiConfig

          t = Milliseconds 2000.0

          newStEvt  = fold applyOp (cfg ^. _operations) Map.empty
    
          calcOp { last: Just od, now: nd } = diff nd od
          calcOp { last: Nothing, now: _  } = Nil

          newOpEvt  = calcOp <$> withLast (debounce t newStEvt)

          runOps    = map anyEvt <<< traverse (flip runAPI apiCfg <<< callPanelAPI houseId roofId)
          apiResEvt = multicast $ keepLatest $ performEvent $ runOps <$> newOpEvt

callPanelAPI :: Int -> UUID -> PanelOperation -> API (Event Unit)
callPanelAPI _ _ (LoadPanels _)            = pure Plus.empty
callPanelAPI houseId _ (AddPanel p)        = createPanel houseId p
callPanelAPI houseId roofId (AddPanels ps) = onlyCallFull ps (toUnfoldable >>> createPanels houseId roofId)
callPanelAPI houseId _ (DelPanel pid)      = deletePanels houseId [pid]
callPanelAPI houseId _ (DelPanels pids)    = onlyCallFull pids (toUnfoldable >>> deletePanels houseId)
callPanelAPI houseId roofId DeleteAll      = deletePanelsInRoof houseId roofId
callPanelAPI houseId _ (UpdatePanels ps)   = onlyCallFull ps (toUnfoldable >>> updatePanels houseId)

-- only call the provided API function if the param is not null
onlyCallFull :: forall f a. Foldable f => f a -> (f a -> API (Event Unit)) -> API (Event Unit)
onlyCallFull ls f = if null ls
                    then pure (pure unit)
                    else f ls
