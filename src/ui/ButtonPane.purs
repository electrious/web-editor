module UI.ButtonPane where

import Prelude hiding (div)

import Specular.Dom.Browser (Attrs)
import Specular.Dom.Element (attrs, classes, text)
import Specular.Dom.Widget (Widget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (dynamic, leftmost, never, switch)
import Specular.FRP as S
import UI.Utils (div, mkAttrs, mkStyle, (:~))


data ButtonClicked = BCSave
                   | BCClose

derive instance eqButtonClicked :: Eq ButtonClicked

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
buttons :: S.Dynamic Boolean -> S.Dynamic Boolean -> Widget (S.Event ButtonClicked)
buttons showSaveDyn showCloseDyn =
    div [classes ["uk-flex", "uk-flex-right"],
         attrs btnsStyle] do
        saveEvt <- switch <$> dynamic (saveBtn <$> showSaveDyn)
        clsEvt <- switch <$> dynamic (closeBtn <$> showCloseDyn)
        
        pure $ leftmost [const BCSave <$> saveEvt,
                         const BCClose <$> clsEvt]

saveBtn :: Boolean -> Widget (S.Event Unit)
saveBtn true  = buttonOnClick (pure $ mkAttrs ["class" :~ "uk-button"]) (text "Save")
saveBtn false = pure never


closeBtn :: Boolean -> Widget (S.Event Unit)
closeBtn true  = buttonOnClick (pure $ mkAttrs ["class" :~ "uk-button uk-margin-left uk-modal-close"]) (text "Close")
closeBtn false = pure never
