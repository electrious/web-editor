module UI.RoofEditorUI where

import Prelude hiding (div)

import Control.Plus (empty)
import Data.Default (class Default, def)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Editor.Common.Lenses (_height, _roofs, _width)
import Editor.Editor (_sizeDyn)
import Editor.EditorMode (EditorMode(..))
import Editor.HouseEditor (ArrayEditParam)
import Editor.SceneEvent (Size, size)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic)
import FRP.Event (Event)
import Model.Roof.RoofPlate (RoofEdited)
import Specular.Dom.Browser (Attrs)
import Specular.Dom.Element (attr, attrs, attrsD, class_, classes, el, text)
import Specular.Dom.Widget (Widget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (dynamic, filterEvent, filterJustEvent, holdDyn, leftmost, never, switch)
import Specular.FRP as S
import UI.ArrayEditorUI (ArrayEditorUIOpt, arrayEditorPane)
import UI.Bridge (fromUIDyn, fromUIEvent, toUIDyn, toUIEvent)
import UI.ConfirmDialog (ConfirmResult(..), confirmDialog)
import UI.RoofInstructions (roofInstructions)
import UI.Utils (div, elA, mkAttrs, mkStyle, (:~))

newtype RoofEditorUIOpt = RoofEditorUIOpt {
    mode     :: Event EditorMode,
    sizeDyn  :: Dynamic Size,
    roofs    :: Dynamic (Maybe (Array RoofEdited)),
    arrayOpt :: ArrayEditorUIOpt
    }

derive instance newtypeRoofEditorUIOpt :: Newtype RoofEditorUIOpt _
instance defaultRoofEditorUIOpt :: Default RoofEditorUIOpt where
    def = RoofEditorUIOpt {
        mode     : empty,
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
    modeDyn    :: Dynamic EditorMode
    }

derive instance newtypeRoofEditorUIResult :: Newtype RoofEditorUIResult _

_arrayParam :: forall t a r. Newtype t { arrayParam :: a | r } => Lens' t a
_arrayParam = _Newtype <<< prop (SProxy :: SProxy "arrayParam")

_editorOp :: forall t a r. Newtype t { editorOp :: a | r } => Lens' t a
_editorOp = _Newtype <<< prop (SProxy :: SProxy "editorOp")

roofEditorUI :: RoofEditorUIOpt -> Widget RoofEditorUIResult
roofEditorUI opt = do
    let style s = mkStyle [ "position"       :~ "absolute",
                            "width"          :~ (show (s ^. _width) <> "px"),
                            "height"         :~ (show (s ^. _height) <> "px"),
                            "left"           :~ "0",
                            "top"            :~ "0",
                            "pointer-events" :~ "none" ]
    sizeD <- liftEffect $ toUIDyn $ opt ^. _sizeDyn
    div [attrsD $ style <$> sizeD, class_ "uk_inline"] do
        rsDyn <- liftEffect $ toUIDyn $ opt ^. _roofs

        Tuple param modeUIDyn <- editorPane opt
        opEvt <- buttons modeUIDyn rsDyn
        confOpEvt <- fromUIEvent =<< askConfirm opEvt

        modeDyn <- fromUIDyn modeUIDyn

        pure $ RoofEditorUIResult {
            arrayParam : param,
            editorOp   : confOpEvt,
            modeDyn    : modeDyn
            }

shadowStyle :: String
shadowStyle = "0 3px 1px -2px rgba(0, 0, 0, 0.2), 0 2px 2px 0 rgba(0, 0, 0, 0.14), 0 1px 5px 0 rgba(0, 0, 0, 0.06)"

editorPane :: RoofEditorUIOpt -> Widget (Tuple ArrayEditParam (S.Dynamic EditorMode))
editorPane opt =
    div [classes ["uk-overlay", "uk-overlay-default", "uk-padding-small", "uk-position-top-left"],
         attrs $ mkStyle ["box-shadow" :~ shadowStyle,
                          "pointer-events" :~ "auto" ]] do
        modeEvt  <- headerTab
        arrParam <- body $ opt ^. _arrayOpt

        mEvt <- liftEffect $ toUIEvent $ opt ^. _mode
        modeDyn <- holdDyn Showing $ leftmost [modeEvt, mEvt]
        
        pure $ Tuple arrParam modeDyn

-- header tab of the pane switcher between Array and Roof editing
headerTab :: Widget (S.Event EditorMode)
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


data EditorUIOp = Save (Array RoofEdited)
                | Close

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
    "position" :~ "absolute",
    "width"    :~ "180px",
    "top"      :~ "20px",
    "right"    :~ "20px",
    "z-index"  :~ "10"
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
    let confSave (Just rs) = do
            e <- confirmDialog (text "A.I. will redesign the solar system when roof plates are edited")
            pure $ confirm rs <$> e
        confSave _ = pure never

        confirm rs Confirmed = Just rs
        confirm _  Cancelled = Nothing
        
    opDyn <- holdDyn Nothing $ Just <$> rsEvt
    e <- switch <$> dynamic (confSave <$> opDyn)
    pure $ filterJustEvent e
