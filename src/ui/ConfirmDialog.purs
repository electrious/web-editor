module UI.ConfirmDialog where

import Prelude hiding (div)

import Specular.Dom.Element (class_, classes, el, text)
import Specular.Dom.Widget (Widget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (Event, leftmost)
import UI.Utils (div, mkAttrs, (:~))

data ConfirmResult = Confirmed
                   | Cancelled

confirmDialog :: Widget Unit -> Widget (Event ConfirmResult)
confirmDialog child =
    div [class_ "we-confirm-dialog"] $
        el "form" [] do
            div [class_ "uk-modal-body"] child
            div [classes ["uk-modal-footer", "uk-text-center"]] do
                confirmE <- buttonOnClick (pure $ mkAttrs ["class" :~ "uk-button uk-button-primary"]) $ text "Continue"
                cancelE  <- buttonOnClick (pure $ mkAttrs ["class" :~ "uk-button uk-button-default"]) $ text "Cancel"
                pure $ leftmost [const Confirmed <$> confirmE,
                                 const Cancelled <$> cancelE]
