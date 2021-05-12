module UI.RoofEditorUI where

import Prelude hiding (div)

import API (APIConfig, runAPI)
import API.Roofplate (buildRoofplates)
import Control.Alt ((<|>))
import Data.Default (class Default, def)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Editor.Common.Lenses (_apiConfig, _height, _houseId, _modeDyn, _roofs, _width)
import Editor.Editor (_sizeDyn)
import Editor.EditorMode (EditorMode(..))
import Editor.HouseEditor (ArrayEditParam)
import Editor.SceneEvent (Size, size)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic)
import FRP.Event (Event, keepLatest)
import FRP.Event.Extra (multicast, performEvent)
import Model.Roof.RoofPlate (RoofEdited)
import Specular.Dom.Element (attrs, attrsD, classWhenD, class_, classes, el)
import Specular.Dom.Widget (Widget)
import Specular.FRP (filterEvent, filterJustEvent, leftmost, tagDyn)
import Specular.FRP as S
import UI.ArrayEditorUI (ArrayEditorUIOpt, arrayEditorPane)
import UI.Bridge (fromUIEvent, toUIDyn)
import UI.ButtonPane (ButtonClicked(..), buttons)
import UI.ConfirmDialog (askConfirm)
import UI.EditorUIOp (EditorUIOp(..))
import UI.RoofInstructions (roofInstructions)
import UI.Utils (div, elA, mkStyle, (:~))

newtype RoofEditorUIOpt = RoofEditorUIOpt {
    houseId   :: Int,
    apiConfig :: APIConfig,
    modeDyn   :: Dynamic EditorMode,
    sizeDyn   :: Dynamic Size,
    roofs     :: Dynamic (Maybe (Array RoofEdited)),
    arrayOpt  :: ArrayEditorUIOpt
    }

derive instance newtypeRoofEditorUIOpt :: Newtype RoofEditorUIOpt _
instance defaultRoofEditorUIOpt :: Default RoofEditorUIOpt where
    def = RoofEditorUIOpt {
        houseId   : 0,
        apiConfig : def,
        modeDyn   : pure Showing,
        sizeDyn   : pure (size 10 10),
        roofs     : pure Nothing,
        arrayOpt  : def
        }

_mode :: forall t a r. Newtype t { mode :: a | r } => Lens' t a
_mode = _Newtype <<< prop (SProxy :: SProxy "mode")

_arrayOpt :: forall t a r. Newtype t { arrayOpt :: a | r } => Lens' t a
_arrayOpt = _Newtype <<< prop (SProxy :: SProxy "arrayOpt")


newtype RoofEditorUIResult = RoofEditorUIResult {
    arrayParam :: ArrayEditParam,
    editorOp   :: Event EditorUIOp,
    mode       :: Event EditorMode
    }

derive instance newtypeRoofEditorUIResult :: Newtype RoofEditorUIResult _

_arrayParam :: forall t a r. Newtype t { arrayParam :: a | r } => Lens' t a
_arrayParam = _Newtype <<< prop (SProxy :: SProxy "arrayParam")

_editorOp :: forall t a r. Newtype t { editorOp :: a | r } => Lens' t a
_editorOp = _Newtype <<< prop (SProxy :: SProxy "editorOp")

roofEditorUI :: RoofEditorUIOpt -> Widget RoofEditorUIResult
roofEditorUI opt = do
    let style s m = mkStyle [ "position"       :~ "absolute",
                              "width"          :~ (show (s ^. _width) <> "px"),
                              "height"         :~ (show (s ^. _height) <> "px"),
                              "left"           :~ "0",
                              "top"            :~ "0",
                              "pointer-events" :~ "none",
                              "display"        :~ display m
                              ]
        display Showing = "none"
        display _       = "inline"
    
    sizeD <- liftEffect $ toUIDyn $ opt ^. _sizeDyn
    modeD <- liftEffect $ toUIDyn $ opt ^. _modeDyn
    div [attrsD $ style <$> sizeD <*> modeD] do
        rsDyn <- liftEffect $ toUIDyn $ opt ^. _roofs

        Tuple param modeUIEvt <- editorPane opt modeD

        let showSaveDyn = (&&) <$> ((==) RoofEditing <$> modeD) <*> (isJust <$> rsDyn)
            showCloseDyn = ((==) Showing) <$> modeD
        opEvt <- buttons showSaveDyn showCloseDyn

        -- Save events means the save button clicked here
        let saveClickedEvt = filterEvent ((==) BCSave) opEvt
        closeEvt   <- fromUIEvent $ const Close <$> filterEvent ((==) BCClose) opEvt
        
        canSaveEvt <- askConfirm $ const unit <$> saveClickedEvt
        toSaveEvt  <- fromUIEvent $ filterJustEvent $ tagDyn rsDyn $ const unit <$> canSaveEvt

        -- run API to save the new roofplates edited
        let apiCfg = opt ^. _apiConfig
            hid    = opt ^. _houseId
            -- Save event here means the roofs are saved
            savedEvt = multicast $ keepLatest $ performEvent (flip runAPI apiCfg <<< buildRoofplates hid <$> toSaveEvt)
            roofSaved = const RoofSaved <$> savedEvt
            
        modeEvt <- fromUIEvent modeUIEvt

        pure $ RoofEditorUIResult {
            arrayParam : param,
            editorOp   : multicast $ roofSaved <|> closeEvt,
            mode       : multicast modeEvt
            }
    
editorPane :: RoofEditorUIOpt -> S.Dynamic EditorMode -> Widget (Tuple ArrayEditParam (S.Event EditorMode))
editorPane opt modeDyn =
    div [classes ["uk-overlay", "uk-overlay-default", "uk-padding-small", "uk-position-top-left", "uk-box-shadow-medium"],
         attrs $ mkStyle ["pointer-events" :~ "auto"]] do
        modeEvt  <- headerTab modeDyn
        arrParam <- body (opt ^. _arrayOpt) modeDyn
        
        pure $ Tuple arrParam modeEvt

-- header tab of the pane switcher between Array and Roof editing
headerTab :: S.Dynamic EditorMode -> Widget (S.Event EditorMode)
headerTab modeDyn = el "ul" [classes ["uk-subnav", "uk-subnav-pill"]] do
    arrEvt  <- el "li" [classWhenD ((==) ArrayEditing <$> modeDyn) "uk-active"] $ elA "Edit Arrays" "#"
    roofEvt <- el "li" [classWhenD ((==) RoofEditing <$> modeDyn) "uk-active"] $ elA "Edit Roofs" "#"

    pure $ leftmost [const ArrayEditing <$> arrEvt,
                     const RoofEditing <$> roofEvt]

-- body part of the switcher between array editing and roof editing UI
body :: ArrayEditorUIOpt -> S.Dynamic EditorMode -> Widget ArrayEditParam
body opt modeDyn =
    el "ul" [class_ "uk-switcher"] do
        res <- el "li" [classWhenD ((==) ArrayEditing <$> modeDyn) "uk-active"] $ arrayEditorPane opt
        el "li" [classWhenD ((==) RoofEditing <$> modeDyn) "uk-active"] roofInstructions
        pure res
