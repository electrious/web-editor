module UI.ButtonPane where

import Prelude hiding (div)

import Control.Alternative (empty)
import Data.Default (class Default, def)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import FRP.Event (Event)
import Specular.Dom.Browser (Attrs)
import Specular.Dom.Element (attrs, classes, text)
import Specular.Dom.Widget (Widget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (dynamic, never, switch)
import Specular.FRP as S
import UI.Bridge (fromUIEvent)
import UI.Utils (div, mkAttrs, mkStyle, (:~))


newtype ButtonsConf = ButtonsConf {
    showSaveDyn  :: S.Dynamic Boolean,
    showCloseDyn :: S.Dynamic Boolean,
    showResetDyn :: S.Dynamic Boolean,
    showUndoDyn  :: S.Dynamic Boolean
    }

derive instance newtypeButtonsConf :: Newtype ButtonsConf _
instance defaultButtonsConf :: Default ButtonsConf where
    def = ButtonsConf {
        showSaveDyn  : pure false,
        showCloseDyn : pure false,
        showResetDyn : pure false,
        showUndoDyn  : pure false
        }

_showCloseDyn :: forall t a r. Newtype t { showCloseDyn :: a | r } => Lens' t a
_showCloseDyn = _Newtype <<< prop (SProxy :: SProxy "showCloseDyn")

_showSaveDyn :: forall t a r. Newtype t { showSaveDyn :: a | r } => Lens' t a
_showSaveDyn = _Newtype <<< prop (SProxy :: SProxy "showSaveDyn")

_showResetDyn :: forall t a r. Newtype t { showResetDyn :: a | r } => Lens' t a
_showResetDyn = _Newtype <<< prop (SProxy :: SProxy "showResetDyn")

_showUndoDyn :: forall t a r. Newtype t { showUndoDyn :: a | r } => Lens' t a
_showUndoDyn = _Newtype <<< prop (SProxy :: SProxy "showUndoDyn")


newtype ButtonsPane = ButtonsPane {
    close :: Event Unit,
    save  :: Event Unit,
    reset :: Event Unit,
    undo  :: Event Unit
    }

derive instance newtypeButtonsPane :: Newtype ButtonsPane _
instance defaultButtonsPane :: Default ButtonsPane where
    def = ButtonsPane {
        close : empty,
        save  : empty,
        reset : empty,
        undo  : empty
        }

_close :: forall t a r. Newtype t { close :: a | r } => Lens' t a
_close = _Newtype <<< prop (SProxy :: SProxy "close")

_save :: forall t a r. Newtype t { save :: a | r } => Lens' t a
_save = _Newtype <<< prop (SProxy :: SProxy "save")

_reset :: forall t a r. Newtype t { reset :: a | r } => Lens' t a
_reset = _Newtype <<< prop (SProxy :: SProxy "reset")

_undo :: forall t a r. Newtype t { undo :: a | r } => Lens' t a
_undo = _Newtype <<< prop (SProxy :: SProxy "undo")
                    
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
buttons :: ButtonsConf -> Widget ButtonsPane
buttons conf =
    div [classes ["uk-flex", "uk-flex-right"],
         attrs btnsStyle] do
        undoEvtUI <- switch <$> dynamic (undoBtn <$> conf ^. _showUndoDyn)
        rstEvtUI  <- switch <$> dynamic (resetBtn <$> conf ^. _showResetDyn)
        saveEvtUI <- switch <$> dynamic (saveBtn <$> conf ^. _showSaveDyn)
        clsEvtUI  <- switch <$> dynamic (closeBtn <$> conf ^. _showCloseDyn)

        saveEvt <- fromUIEvent saveEvtUI
        clsEvt  <- fromUIEvent clsEvtUI
        rstEvt  <- fromUIEvent rstEvtUI
        undoEvt <- fromUIEvent undoEvtUI
        
        pure $ def # _save  .~ saveEvt
                   # _close .~ clsEvt
                   # _reset .~ rstEvt
                   # _undo  .~ undoEvt

saveBtn :: Boolean -> Widget (S.Event Unit)
saveBtn true  = buttonOnClick (pure $ mkAttrs ["class" :~ "uk-button uk-margin-left"]) (text "Save")
saveBtn false = pure never


closeBtn :: Boolean -> Widget (S.Event Unit)
closeBtn true  = buttonOnClick (pure $ mkAttrs ["class" :~ "uk-button uk-margin-left uk-modal-close"]) (text "Close")
closeBtn false = pure never


resetBtn :: Boolean -> Widget (S.Event Unit)
resetBtn true  = buttonOnClick (pure $ mkAttrs ["class" :~ "uk-button uk-margin-left"]) (text "Reset")
resetBtn false = pure never


undoBtn :: Boolean -> Widget (S.Event Unit)
undoBtn true  = buttonOnClick (pure $ mkAttrs ["class" :~ "uk-button"]) (text "Undo")
undoBtn false = pure never
