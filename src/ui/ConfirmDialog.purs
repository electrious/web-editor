module UI.ConfirmDialog where

import Prelude hiding (div)

import Specular.Dom.Browser (Attrs)
import Specular.Dom.Element (attrs, class_, classes, el, text)
import Specular.Dom.Widget (Widget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (Event, leftmost)
import UI.Utils (div, mkAttrs, mkStyle, (:~))

data ConfirmResult = Confirmed
                   | Cancelled


dialogAttr :: Attrs
dialogAttr = mkStyle ["position"         :~ "absolute",
                      "top"              :~ "40%",
                      "left"             :~ "50%",
                      "width"            :~ "500px",
                      "height"           :~ "120px",
                      "margin-left"      :~ "-250px",
                      "margin-top"       :~ "-60px",
                      "z-index"          :~ "2500",
                      "background-color" :~ "white",
                      "border-radius"    :~ "5px",
                      "box-shadow"       :~ "0 3px 1px -2px rgba(0, 0, 0, 0.2), 0 2px 2px 0 rgba(0, 0, 0, 0.14), 0 1px 5px 0 rgba(0, 0, 0, 0.06)",
                      "pointer-events"   :~ "auto"
                     ]

confirmDialog :: Widget Unit -> Widget (Event ConfirmResult)
confirmDialog child =
    div [attrs dialogAttr] $
        el "form" [] do
            div [class_ "uk-modal-body"] child
            div [classes ["uk-modal-footer", "uk-text-center"]] do
                confirmE <- buttonOnClick (pure $ mkAttrs ["class" :~ "uk-button uk-button-primary"]) $ text "Continue"
                cancelE  <- buttonOnClick (pure $ mkAttrs ["class" :~ "uk-button uk-button-default"]) $ text "Cancel"
                pure $ leftmost [const Confirmed <$> confirmE,
                                 const Cancelled <$> cancelE]
