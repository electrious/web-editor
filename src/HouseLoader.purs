module Editor.HouseLoader (editHouse, House, _loaded, _screenshot, _roofUpdate) where

import Prelude hiding (add)

import API.Panel (loadPanels)
import API.Racking (loadRacking)
import API.Roofplate (loadRoofplates)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ask)
import Data.Either (Either(..))
import Data.Lens (Lens', view, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_alignment, _houseId, _leadId, _orientation, _panels, _roofRackings, _wrapper)
import Editor.Disposable (dispose)
import Editor.Editor (Editor, _canvas, addDisposable)
import Editor.House (loadHouseModel)
import Editor.HouseEditor (ArrayEditParam, HouseConfig, HouseEditor, _arrayEditParam, _dataServer, _roofplates, _screenshotDelay, performEditorEvent, runAPIInEditor, runHouseEditor)
import Editor.PanelLayer (_serverUpdated)
import Editor.RoofManager (_editedRoofs, createRoofManager)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Event (Event, create, keepLatest)
import FRP.Event.Extra (delay, multicast, performEvent)
import Model.Roof.Panel (Alignment, Orientation)
import Model.Roof.RoofPlate (RoofEdited)
import Three.Core.Object3D (add)
import Three.Core.WebGLRenderer (toDataUrl)

editHouse :: Editor -> HouseConfig -> Effect House
editHouse scene houseCfg = runHouseEditor (loadHouse scene (houseCfg ^. _arrayEditParam)) houseCfg

newtype House = House {
    loaded        :: Event Unit,
    screenshot    :: Event String,
    roofUpdate    :: Event (Array RoofEdited),

    -- array level state events
    alignment     :: Event (Maybe Alignment),
    orientation   :: Event (Maybe Orientation),
    serverUpdated :: Event Unit
}

derive instance newtypeHouse :: Newtype House _

_loaded :: forall t a r. Newtype t { loaded :: a | r } => Lens' t a
_loaded = _Newtype <<< prop (SProxy :: SProxy "loaded")

_screenshot :: forall t a r. Newtype t { screenshot :: a | r } => Lens' t a
_screenshot = _Newtype <<< prop (SProxy :: SProxy "screenshot")

_roofUpdate :: forall t a r. Newtype t { roofUpdate :: a | r } => Lens' t a
_roofUpdate = _Newtype <<< prop (SProxy :: SProxy "roofUpdate")

loadHouse :: Editor -> ArrayEditParam -> HouseEditor House
loadHouse editor param = do
    cfg <- ask

    { event: loadedEvt, push: loadedFunc } <- liftEffect create
    
    -- load house model, roofs, panels and racking data
    let houseId   = cfg ^. _houseId
        defRoofs  = cfg ^. _roofplates
        defPanels = cfg ^. _panels

    hmEvt     <- liftEffect $ loadHouseModel (cfg ^. _dataServer) (cfg ^. _leadId)
    roofsEvt  <- if defRoofs == []
                 then runAPIInEditor $ loadRoofplates houseId
                 else pure $ pure defRoofs
    panelsEvt <- if defPanels == []
                 then runAPIInEditor $ loadPanels houseId
                 else pure $ pure defPanels
    racksEvt  <- runAPIInEditor $ loadRacking houseId

    -- extract the roof racking map data
    let roofRackDatEvt = extrRoofRack <$> racksEvt
        extrRoofRack res = case runExcept res of
                            Left _ -> Map.empty
                            Right v -> v ^. _roofRackings
    
        buildRoofMgr hmd roofsDat panelsDat roofRackData = do
            mgr <- createRoofManager param hmd roofsDat panelsDat roofRackData
            liftEffect do
                add (hmd ^. _wrapper) editor
                add (mgr ^. _wrapper) editor
                addDisposable (dispose mgr) editor
            
                loadedFunc unit
            
            pure mgr
        
        getScreenshot _ = toDataUrl "image/png" (editor ^. _canvas)
    
    mgrEvt <- multicast <$> performEditorEvent (buildRoofMgr <$> hmEvt <*> roofsEvt <*> panelsEvt <*> roofRackDatEvt)

    pure $ House {
        loaded        : loadedEvt,
        screenshot    : performEvent $ getScreenshot     <$> delay (cfg ^. _screenshotDelay) loadedEvt,
        roofUpdate    : keepLatest $ view _editedRoofs   <$> mgrEvt,

        alignment     : keepLatest $ view _alignment     <$> mgrEvt,
        orientation   : keepLatest $ view _orientation   <$> mgrEvt,
        serverUpdated : keepLatest $ view _serverUpdated <$> mgrEvt
    }
