module Editor.Editor (EditorConfig(..), editHouse, House, _loaded, _screenshot, _roofUpdate, _elem, _sizeDyn, _flyCameraTarget) where

import Prelude hiding (add)

import API.Racking (loadRacking)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ask)
import Control.Plus (empty)
import Data.Default (class Default)
import Data.Either (Either(..))
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Editor.Common.Lenses (_houseId, _leadId, _modeDyn, _roofRackings, _wrapper)
import Editor.Disposable (dispose)
import Editor.EditorMode (EditorMode(..))
import Editor.House (loadHouseModel)
import Editor.HouseEditor (HouseConfig, HouseEditor, _dataServer, _screenshotDelay, performEditorEvent, runAPIInEditor, runHouseEditor)
import Editor.RoofManager (_editedRoofs, createRoofManager)
import Editor.SceneEvent (Size, size)
import Editor.WebEditor (WebEditor, _canvas, addDisposable, addToScene, createScene, renderLoop)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, step)
import FRP.Event (Event, create, keepLatest)
import FRP.Event.Extra (delay, performEvent)
import Model.Roof.RoofPlate (RoofEdited)
import Three.Core.WebGLRenderer (toDataUrl)
import Three.Math.Vector (Vector3)
import Web.DOM (Element)
import Web.HTML (window)

newtype EditorConfig = EditorConfig {
    elem            :: Maybe Element,
    sizeDyn         :: Dynamic Size,
    modeDyn         :: Dynamic EditorMode,
    flyCameraTarget :: Dynamic (Maybe Vector3)
}

derive instance newtypeEditorConfig :: Newtype EditorConfig _
instance defaultEditorConfig :: Default EditorConfig where
    def = EditorConfig {
        elem            : Nothing,
        sizeDyn         : step (size 800 600) empty,
        modeDyn         : step Showing empty,
        flyCameraTarget : step Nothing empty
    }

_elem :: forall t a r. Newtype t { elem :: a | r } => Lens' t a
_elem = _Newtype <<< prop (SProxy :: SProxy "elem")

_sizeDyn :: forall t a r. Newtype t { sizeDyn :: a | r } => Lens' t a
_sizeDyn = _Newtype <<< prop (SProxy :: SProxy "sizeDyn")

_flyCameraTarget :: forall t a r. Newtype t { flyCameraTarget :: a | r } => Lens' t a
_flyCameraTarget = _Newtype <<< prop (SProxy :: SProxy "flyCameraTarget")

-- | editHouse will create the Web Editor instance
editHouse :: EditorConfig -> HouseConfig -> Effect (Maybe House)
editHouse cfg houseCfg = traverse mkEditor (cfg ^. _elem)
    where mkEditor elem = do
            scene <- createScene (cfg ^. _sizeDyn)
                                 (cfg ^. _modeDyn)
                                 (cfg ^. _flyCameraTarget)
                                 elem
            -- start the rednerring
            window >>= renderLoop scene
            
            runHouseEditor (loadHouse scene) houseCfg

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
                            Left _ -> Map.empty
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
