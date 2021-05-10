module UI.RoofEditorUI where

import Prelude hiding (div)

import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Editor.Common.Lenses (_roofs)
import Editor.EditorMode (EditorMode(..))
import Editor.HouseEditor (ArrayEditParam)
import Model.Roof.RoofPlate (RoofPlate)
import Specular.Dom.Browser (Attrs)
import Specular.Dom.Element (attr, attrs, class_, classes, el, text)
import Specular.Dom.Widget (Widget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (Dynamic, Event, dynamic, filterEvent, filterJustEvent, holdDyn, leftmost, never, switch)
import UI.ArrayEditorUI (ArrayEditorUIOpt, arrayEditorPane)
import UI.ConfirmDialog (ConfirmResult(..), confirmDialog)
import UI.RoofInstructions (roofInstructions)
import UI.Utils (div, elA, mkAttrs, mkStyle, (:~))

newtype RoofEditorUIOpt = RoofEditorUIOpt {
    mode     :: Event EditorMode,
    roofs    :: Dynamic (Maybe (Array RoofPlate)),
    arrayOpt :: ArrayEditorUIOpt
    }

derive instance newtypeRoofEditorUIOpt :: Newtype RoofEditorUIOpt _

_mode :: forall t a r. Newtype t { mode :: a | r } => Lens' t a
_mode = _Newtype <<< prop (SProxy :: SProxy "mode")

_arrayOpt :: forall t a r. Newtype t { arrayOpt :: a | r } => Lens' t a
_arrayOpt = _Newtype <<< prop (SProxy :: SProxy "arrayOpt")


newtype RoofEditorUIResult = RoofEditorUIResult {
    arrayParam :: ArrayEditParam,
    editorOp   :: Event EditorUIOp,
    modeDyn    :: Dynamic EditorMode
    }

derive instance newtypeRoofEditorUIResult :: Newtype RoofEditorUIResult _

_arrayParam :: forall t a r. Newtype t { arrayParam :: a | r } => Lens' t a
_arrayParam = _Newtype <<< prop (SProxy :: SProxy "arrayParam")

_editorOp :: forall t a r. Newtype t { editorOp :: a | r } => Lens' t a
_editorOp = _Newtype <<< prop (SProxy :: SProxy "editorOp")

roofEditorUI :: RoofEditorUIOpt -> Widget RoofEditorUIResult
roofEditorUI opt = do
    Tuple param modeDyn <- editorPane opt
    opEvt <- buttons modeDyn (opt ^. _roofs)
    confOpEvt <- askConfirm opEvt

    pure $ RoofEditorUIResult {
        arrayParam : param,
        editorOp   : confOpEvt,
        modeDyn    : modeDyn
        }

shadowStyle :: String
shadowStyle = "0 3px 1px -2px rgba(0, 0, 0, 0.2), 0 2px 2px 0 rgba(0, 0, 0, 0.14), 0 1px 5px 0 rgba(0, 0, 0, 0.06)"

editorPane :: RoofEditorUIOpt -> Widget (Tuple ArrayEditParam (Dynamic EditorMode))
editorPane opt =
    div [classes ["uk-overlay", "uk-overlay-default", "uk-padding-small", "uk-position-top-left"],
         attrs $ mkStyle ["box-shadow" :~ shadowStyle ]] do
        modeEvt  <- headerTab
        arrParam <- body $ opt ^. _arrayOpt

        modeDyn <- holdDyn Showing $ leftmost [modeEvt, opt ^. _mode]
        
        pure $ Tuple arrParam modeDyn

-- header tab of the pane switcher between Array and Roof editing
headerTab :: Widget (Event EditorMode)
headerTab = el "ul" [classes ["uk-subnav", "uk-subnav-pill"],
                     attr "uk-switcher" ""] do
    arrEvt  <- el "li" [] $ elA "Edit Arrays" "#"
    roofEvt <- el "li" [] $ elA "Edit Roofs" "#"

    pure $ leftmost [const ArrayEditing <$> arrEvt,
                     const RoofEditing <$> roofEvt]

-- body part of the switcher between array editing and roof editing UI
body :: ArrayEditorUIOpt -> Widget ArrayEditParam
body opt =
    el "ul" [class_ "uk-switcher"] do
        res <- el "li" [] $ arrayEditorPane opt
        el "li" [] roofInstructions
        pure res


data EditorUIOp = Save (Array RoofPlate)
                | Close

isSave :: EditorUIOp -> Boolean
isSave (Save _) = true
isSave _        = false

isClose :: EditorUIOp -> Boolean
isClose Close = true
isClose _     = false

getSave :: EditorUIOp -> Maybe (Array RoofPlate)
getSave (Save rs) = Just rs
getSave _         = Nothing

btnsStyle :: Attrs
btnsStyle = mkStyle [
    "position" :~ "absolute",
    "width"    :~ "180px",
    "top"      :~ "20px",
    "right"    :~ "20px",
    "z-index"  :~ "10"
    ]

-- buttons to show on the top right corner of the editor
buttons :: Dynamic EditorMode -> Dynamic (Maybe (Array RoofPlate)) -> Widget (Event EditorUIOp)
buttons modeDyn roofsDyn =
    div [classes ["uk-flex", "uk-flex-right"],
         attrs btnsStyle] do
        let editingRoofDyn = (==) RoofEditing <$> modeDyn
            showCloseDyn = (/=) Showing <$> modeDyn
        saveEvt <- switch <$> dynamic (saveBtn <$> editingRoofDyn <*> roofsDyn)
        clsEvt <- switch <$> dynamic (closeBtn <$> showCloseDyn)
        pure $ leftmost [saveEvt, clsEvt]

saveBtn :: Boolean -> Maybe (Array RoofPlate) -> Widget (Event EditorUIOp)
saveBtn true (Just roofs) = map (const $ Save roofs) <$> buttonOnClick attD (text "Save")
    where attD = pure $ mkAttrs ["class" :~ "uk-button"]
saveBtn true Nothing      = pure never
saveBtn false _           = pure never


closeBtn :: Boolean -> Widget (Event EditorUIOp)
closeBtn true  = map (const Close) <$> buttonOnClick attD (text "Close")
    where attD = pure $ mkAttrs ["class" :~ "uk-button uk-margin-left uk-modal-close"]
closeBtn false = pure never

-- show confirm dialog to let user confirm if save
askConfirm :: Event EditorUIOp -> Widget (Event EditorUIOp)
askConfirm evt = do
    let saveEvt = filterEvent isSave evt
        clsEvt  = filterEvent isClose evt
    confirmed <- askConfirmToSave $ filterJustEvent $ getSave <$> saveEvt
    pure $ leftmost [Save <$> confirmed, clsEvt]

askConfirmToSave :: Event (Array RoofPlate) -> Widget (Event (Array RoofPlate))
askConfirmToSave rsEvt = do
    let confSave (Just rs) = do
            e <- confirmDialog (text "A.I. will redesign the solar system when roof plates are edited")
            pure $ confirm rs <$> e
        confSave _ = pure never

        confirm rs Confirmed = Just rs
        confirm _  Cancelled = Nothing
        
    opDyn <- holdDyn Nothing $ Just <$> rsEvt
    e <- switch <$> dynamic (confSave <$> opDyn)
    pure $ filterJustEvent e
