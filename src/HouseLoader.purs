module Editor.HouseLoader (editHouse, House(..), _loaded, _screenshot, _roofUpdate) where

import Prelude hiding (add)

import API.Panel (loadPanels)
import API.Roofplate (loadRoofplates)
import Control.Alt ((<|>))
import Control.Monad.Reader (ask)
import Data.Compactable (compact)
import Data.Default (def)
import Data.Filterable (filter)
import Data.Lens (Lens', view, (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype)
import Type.Proxy (Proxy(..))
import Editor.Common.Lenses (_alignment, _apiConfig, _houseId, _leadId, _modeDyn, _orientation, _panels, _parent, _roofs, _wrapper)
import Taihe.Disposable (dispose)
import Editor.Editor (Editor, _canvas, _sizeDyn, addDisposable, setMode)
import Editor.EditorMode (EditorMode(..))
import Editor.House (loadHouseModel)
import Editor.HouseEditor (ArrayEditParam, HouseConfig, HouseEditor, _dataServer, _heatmap, _roofplates, _screenshotDelay, performEditorEvent, runAPIInEditor, runHouseEditor)
import Editor.PanelLayer (_serverUpdated)
import Editor.PanelNode (PanelOpacity(..))
import Editor.Rendering.PanelRendering (_opacity)
import Editor.RoofManager (_arrayEvents, _editedRoofs, createRoofManager)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (step)
import FRP.Event (Event, create, keepLatest, subscribe)
import FRP.Event.Extra (delay, multicast, performEvent)
import Model.ActiveMode (ActiveMode(..), fromBoolean)
import Model.Roof.Panel (Alignment(..), Orientation)
import Model.Roof.RoofPlate (RoofEdited)
import Specular.Dom.Widget (runMainWidgetInNode)
import Three.Core.Object3D (add)
import Three.Core.WebGLRenderer (toDataUrl)
import UI.ArrayEditorUI (_alignmentEnabled)
import UI.EditorUIOp (EditorUIOp(..))
import UI.RoofEditorUI (_arrayOpt, _arrayParam, _editorOp, _mode, roofEditorUI)
import Unsafe.Coerce (unsafeCoerce)

newtype House = House {
    loaded        :: Event Unit,
    editorOp      :: Event EditorUIOp,
    screenshot    :: Event String
}

derive instance newtypeHouse :: Newtype House _

editHouse :: Editor -> HouseConfig -> Event EditorMode -> Effect House
editHouse editor houseCfg inModeEvt = do
    -- events to send value from UI back to the house editor
    { event: alignEvt, push : pushAlign } <- create
    { event: orientEvt, push: pushOrient } <- create
    { event: opEvt, push: pushOp } <- create
    { event: hmEvt, push: pushHeatmap } <- create
    { event: newModeEvt, push: pushMode } <- create
    { event: savedEvt, push: pushSaved } <- create

    -- setup param for house editor
    let arrayEditParam = def # _alignment .~ alignEvt
                             # _orientation .~ orientEvt
                             # _opacity .~ opEvt
                             # _heatmap .~ hmEvt

        modeEvt = multicast $ newModeEvt <|> inModeEvt
        modeDyn = step Showing modeEvt

    void $ subscribe modeEvt (setMode editor)
    
    houseLoaded <- runHouseEditor (loadHouse editor arrayEditParam) $ houseCfg # _modeDyn .~ modeDyn
    let arrSavedEvt = const ArraySaved <$> houseLoaded ^. _serverUpdated
        -- setup the roof editor UI
        parentEl = unsafeCoerce $ editor ^. _parent

        newAlignEvt = houseLoaded ^. _alignment

        arrayUIOpt = def # _alignment        .~ step Grid (compact newAlignEvt)
                         # _alignmentEnabled .~ step Inactive (fromBoolean <<< isJust <$> newAlignEvt)
                         # _opacity          .~ step Opaque opEvt
                         # _heatmap          .~ step false hmEvt

        roofsDyn = step Nothing $ (Just <$> houseLoaded ^. _roofUpdate) <|> (const Nothing <$> savedEvt)

        opt = def # _houseId  .~ (houseCfg ^. _houseId)
                  # _apiConfig .~ (houseCfg ^. _apiConfig)
                  # _modeDyn  .~ modeDyn
                  # _sizeDyn  .~ (editor ^. _sizeDyn)
                  # _roofs    .~ roofsDyn
                  # _arrayOpt .~ arrayUIOpt

    uiRes <- runMainWidgetInNode parentEl $ roofEditorUI opt

    -- push back the events
    let arrayParam = uiRes ^. _arrayParam
        editorOpEvt = uiRes ^. _editorOp
    void $ subscribe (arrayParam ^. _alignment) pushAlign
    void $ subscribe (arrayParam ^. _orientation) pushOrient
    void $ subscribe (arrayParam ^. _opacity) pushOp
    void $ subscribe (arrayParam ^. _heatmap) pushHeatmap

    void $ subscribe (uiRes ^. _mode) pushMode
    void $ subscribe (filter ((==) RoofSaved) editorOpEvt) pushSaved
    
    pure $ House {
        loaded     : houseLoaded ^. _loaded,
        editorOp   : editorOpEvt <|> arrSavedEvt,
        screenshot : houseLoaded ^. _screenshot
    }


newtype HouseLoaded = HouseLoaded {
    loaded        :: Event Unit,
    screenshot    :: Event String,
    roofUpdate    :: Event (Array RoofEdited),

    -- array level state events
    alignment     :: Event (Maybe Alignment),
    orientation   :: Event (Maybe Orientation),
    serverUpdated :: Event Unit
}

derive instance newtypeHouseLoaded :: Newtype HouseLoaded _

_loaded :: forall t a r. Newtype t { loaded :: a | r } => Lens' t a
_loaded = _Newtype <<< prop (Proxy :: Proxy "loaded")

_screenshot :: forall t a r. Newtype t { screenshot :: a | r } => Lens' t a
_screenshot = _Newtype <<< prop (Proxy :: Proxy "screenshot")

_roofUpdate :: forall t a r. Newtype t { roofUpdate :: a | r } => Lens' t a
_roofUpdate = _Newtype <<< prop (Proxy :: Proxy "roofUpdate")

loadHouse :: Editor -> ArrayEditParam -> HouseEditor HouseLoaded
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

    -- extract the roof racking map data
    let buildRoofMgr hmd roofsDat panelsDat = do
            mgr <- createRoofManager param hmd $ def # _roofs  .~ roofsDat
                                                     # _panels .~ panelsDat
            liftEffect do
                add (hmd ^. _wrapper) editor
                add (mgr ^. _wrapper) editor
                addDisposable (dispose mgr) editor
            
                loadedFunc unit
            
            pure mgr
        
        getScreenshot _ = toDataUrl "image/png" (editor ^. _canvas)
    
    mgrEvt <- multicast <$> performEditorEvent (buildRoofMgr <$> hmEvt <*> roofsEvt <*> panelsEvt)

    pure $ HouseLoaded {
        loaded        : loadedEvt,
        screenshot    : performEvent $ getScreenshot     <$> delay (cfg ^. _screenshotDelay) loadedEvt,
        roofUpdate    : keepLatest $ view _editedRoofs   <$> mgrEvt,

        alignment     : keepLatest $ view (_arrayEvents <<< _alignment) <$> mgrEvt,
        orientation   : keepLatest $ view (_arrayEvents <<< _orientation) <$> mgrEvt,
        serverUpdated : keepLatest $ view (_arrayEvents <<< _serverUpdated) <$> mgrEvt
    }
