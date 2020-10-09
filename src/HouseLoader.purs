module Editor.HouseLoader (editHouse, House, _loaded, _screenshot, _roofUpdate) where

import Prelude hiding (add)

import API.Racking (loadRacking)
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
import Editor.Common.Lenses (_alignment, _houseId, _leadId, _orientation, _roofRackings, _wrapper)
import Editor.Disposable (dispose)
import Editor.Editor (Editor, _canvas, addDisposable)
import Editor.House (loadHouseModel)
import Editor.HouseEditor (ArrayEditParam, HouseConfig, HouseEditor, _arrayEditParam, _dataServer, _screenshotDelay, performEditorEvent, runAPIInEditor, runHouseEditor)
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
    loaded      :: Event Unit,
    screenshot  :: Event String,
    roofUpdate  :: Event (Array RoofEdited),

    -- array level state events
    alignment   :: Event (Maybe Alignment),
    orientation :: Event (Maybe Orientation)
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
    
    -- load house model and racking data
    hmEvt    <- liftEffect $ loadHouseModel (cfg ^. _dataServer) (cfg ^. _leadId)
    racksEvt <- runAPIInEditor $ loadRacking (cfg ^. _houseId)

    -- extract the roof racking map data
    let roofRackDatEvt = extrRoofRack <$> racksEvt
        extrRoofRack res = case runExcept res of
                            Left _ -> Map.empty
                            Right v -> v ^. _roofRackings
    
        buildRoofMgr hmd roofRackData = do
            mgr <- createRoofManager param hmd roofRackData
            liftEffect do
                add (hmd ^. _wrapper) editor
                add (mgr ^. _wrapper) editor
                addDisposable (dispose mgr) editor
            
                loadedFunc unit
            
            pure mgr
        
        getScreenshot _ = toDataUrl "image/png" (editor ^. _canvas)
    
    mgrEvt <- multicast <$> performEditorEvent (buildRoofMgr <$> hmEvt <*> roofRackDatEvt)

    pure $ House {
        loaded      : loadedEvt,
        screenshot  : performEvent $ getScreenshot <$> delay (cfg ^. _screenshotDelay) loadedEvt,
        roofUpdate  : keepLatest $ view _editedRoofs <$> mgrEvt,

        alignment   : keepLatest $ view _alignment   <$> mgrEvt,
        orientation : keepLatest $ view _orientation <$> mgrEvt
    }