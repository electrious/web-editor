module Editor.Editor (createEditor, loadHouse, House, _loaded, _screenshot, _roofUpdate) where

import Prelude hiding (add)

import API.Racking (loadRacking)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ask)
import Data.Either (Either(..))
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (empty)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Editor.Common.Lenses (_houseId, _leadId, _modeDyn, _roofRackings, _wrapper)
import Editor.Disposable (dispose)
import Editor.EditorM (EditorM, _elem, _flyCameraTarget, _sizeDyn)
import Editor.House (loadHouseModel)
import Editor.HouseEditor (HouseEditor, _dataServer, _screenshotDelay, performEditorEvent, runAPIInEditor)
import Editor.RoofManager (_editedRoofs, createRoofManager)
import Editor.WebEditor (WebEditor, _canvas, addDisposable, addToScene, createScene, renderLoop)
import Effect.Class (liftEffect)
import FRP.Event (Event, create, keepLatest)
import FRP.Event.Extra (delay, performEvent)
import Model.Roof.RoofPlate (RoofEdited)
import Three.Core.WebGLRenderer (toDataUrl)
import Web.HTML (window)

-- | createEditor will create the Web Editor instance
createEditor :: EditorM (Maybe WebEditor)
createEditor = do
    cfg <- ask

    let mkEditor elem = do
            scene <- createScene (cfg ^. _sizeDyn)
                                 (cfg ^. _modeDyn)
                                 (cfg ^. _flyCameraTarget)
                                 elem
            -- start the rednerring
            window >>= renderLoop scene
            pure scene
    liftEffect $ traverse mkEditor (cfg ^. _elem)


newtype House = House {
    loaded     :: Event Unit,
    screenshot :: Event String,
    roofUpdate :: Event (Array RoofEdited)
}

derive instance newtypeHouse :: Newtype House _

_loaded :: forall t a r. Newtype t { loaded :: a | r } => Lens' t a
_loaded = _Newtype <<< prop (SProxy :: SProxy "loaded")

_screenshot :: forall t a r. Newtype t { screenshot :: a | r } => Lens' t a
_screenshot = _Newtype <<< prop (SProxy :: SProxy "screenshot")

_roofUpdate :: forall t a r. Newtype t { roofUpdate :: a | r } => Lens' t a
_roofUpdate = _Newtype <<< prop (SProxy :: SProxy "roofUpdate")

loadHouse :: WebEditor -> HouseEditor House
loadHouse editor = do
    cfg <- ask

    { event: loadedEvt, push: loadedFunc } <- liftEffect create
    
    -- load house model and racking data
    hmEvt    <- liftEffect $ loadHouseModel (cfg ^. _dataServer) (cfg ^. _leadId)
    racksEvt <- runAPIInEditor $ loadRacking (cfg ^. _houseId)

    -- extract the roof racking map data
    let roofRackDatEvt = extrRoofRack <$> racksEvt
        extrRoofRack res = case runExcept res of
                            Left _ -> empty
                            Right v -> v ^. _roofRackings
    
        buildRoofMgr hmd roofRackData = do
            mgr <- createRoofManager hmd roofRackData
            liftEffect do
                addToScene (hmd ^. _wrapper) editor
                addToScene (mgr ^. _wrapper) editor
                addDisposable (dispose mgr) editor
            
                loadedFunc unit
            
            pure (mgr ^. _editedRoofs)
        
        getScreenshot _ = toDataUrl "image/png" (editor ^. _canvas)
    
    roofUpdEvt <- keepLatest <$> performEditorEvent (buildRoofMgr <$> hmEvt <*> roofRackDatEvt)

    pure $ House {
        loaded     : loadedEvt,
        screenshot : performEvent $ getScreenshot <$> delay (cfg ^. _screenshotDelay) loadedEvt,
        roofUpdate : roofUpdEvt
    }
