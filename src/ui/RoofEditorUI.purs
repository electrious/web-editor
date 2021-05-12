module UI.RoofEditorUI where

import Prelude hiding (div)

import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Editor.Common.Lenses (_height, _modeDyn, _roofs, _width)
import Editor.Editor (_sizeDyn)
import Editor.EditorMode (EditorMode(..))
import Editor.HouseEditor (ArrayEditParam)
import Editor.SceneEvent (Size, size)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic)
import FRP.Event (Event)
import Model.ActiveMode (ActiveMode(..))
import Model.Roof.RoofPlate (RoofEdited)
import Specular.Dom.Browser (Attrs)
import Specular.Dom.Element (attr, attrs, attrsD, classWhenD, class_, classes, el, text)
import Specular.Dom.Widget (Widget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (dynamic, filterEvent, filterJustEvent, holdDyn, leftmost, never, newEvent, subscribeEvent_, switch, tagDyn)
import Specular.FRP as S
import UI.ArrayEditorUI (ArrayEditorUIOpt, arrayEditorPane)
import UI.Bridge (fromUIEvent, toUIDyn)
import UI.ConfirmDialog (ConfirmResult(..), confirmDialog)
import UI.RoofInstructions (roofInstructions)
import UI.Utils (div, elA, mkAttrs, mkStyle, (:~))

newtype RoofEditorUIOpt = RoofEditorUIOpt {
    modeDyn  :: Dynamic EditorMode,
    sizeDyn  :: Dynamic Size,
    roofs    :: Dynamic (Maybe (Array RoofEdited)),
    arrayOpt :: ArrayEditorUIOpt
    }

derive instance newtypeRoofEditorUIOpt :: Newtype RoofEditorUIOpt _
instance defaultRoofEditorUIOpt :: Default RoofEditorUIOpt where
    def = RoofEditorUIOpt {
        modeDyn  : pure Showing,
        sizeDyn  : pure (size 10 10),
        roofs    : pure Nothing,
        arrayOpt : def
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

        opEvt <- buttons modeD rsDyn
        confOpEvt <- fromUIEvent =<< askConfirm opEvt

        modeEvt <- fromUIEvent modeUIEvt

        pure $ RoofEditorUIResult {
            arrayParam : param,
            editorOp   : confOpEvt,
            mode       : modeEvt
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


data EditorUIOp = Save (Array RoofEdited)
                | Close

derive instance genericEditorUIOp :: Generic EditorUIOp _
instance showEditorUIOp :: Show EditorUIOp where
    show = genericShow

isSave :: EditorUIOp -> Boolean
isSave (Save _) = true
isSave _        = false

isClose :: EditorUIOp -> Boolean
isClose Close = true
isClose _     = false

getSave :: EditorUIOp -> Maybe (Array RoofEdited)
getSave (Save rs) = Just rs
getSave _         = Nothing

btnsStyle :: Attrs
btnsStyle = mkStyle [
    "position"       :~ "absolute",
    "width"          :~ "180px",
    "top"            :~ "20px",
    "right"          :~ "20px",
    "z-index"        :~ "10",
    "pointer-events" :~ "auto"
    ]

-- buttons to show on the top right corner of the editor
buttons :: S.Dynamic EditorMode -> S.Dynamic (Maybe (Array RoofEdited)) -> Widget (S.Event EditorUIOp)
buttons modeDyn roofsDyn =
    div [classes ["uk-flex", "uk-flex-right"],
         attrs btnsStyle] do
        let editingRoofDyn = (==) RoofEditing <$> modeDyn
            showCloseDyn = (/=) Showing <$> modeDyn
        saveEvt <- switch <$> dynamic (saveBtn <$> editingRoofDyn <*> roofsDyn)
        clsEvt <- switch <$> dynamic (closeBtn <$> showCloseDyn)
        pure $ leftmost [saveEvt, clsEvt]

saveBtn :: Boolean -> Maybe (Array RoofEdited) -> Widget (S.Event EditorUIOp)
saveBtn true (Just roofs) = map (const $ Save roofs) <$> buttonOnClick attD (text "Save")
    where attD = pure $ mkAttrs ["class" :~ "uk-button"]
saveBtn true Nothing      = pure never
saveBtn false _           = pure never


closeBtn :: Boolean -> Widget (S.Event EditorUIOp)
closeBtn true  = map (const Close) <$> buttonOnClick attD (text "Close")
    where attD = pure $ mkAttrs ["class" :~ "uk-button uk-margin-left uk-modal-close"]
closeBtn false = pure never

-- show confirm dialog to let user confirm if save
askConfirm :: S.Event EditorUIOp -> Widget (S.Event EditorUIOp)
askConfirm evt = do
    let saveEvt = filterEvent isSave evt
        clsEvt  = filterEvent isClose evt
    confirmed <- askConfirmToSave $ filterJustEvent $ getSave <$> saveEvt
    pure $ leftmost [Save <$> confirmed, clsEvt]

askConfirmToSave :: S.Event (Array RoofEdited) -> Widget (S.Event (Array RoofEdited))
askConfirmToSave rsEvt = do
    opDyn <- holdDyn Nothing $ Just <$> rsEvt

    { event: closeEvt, fire: toClose } <- newEvent

    actDyn <- holdDyn Inactive $ leftmost [const Active <$> rsEvt,
                                           const Inactive <$> closeEvt]
    e <- confirmDialog actDyn (text "A.I. will redesign the solar system when roof plates are edited")

    subscribeEvent_ toClose e

    pure $ filterJustEvent $ tagDyn opDyn $ const unit <$> filterEvent ((==) Confirmed) e
