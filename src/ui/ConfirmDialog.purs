module UI.ConfirmDialog where

import Prelude hiding (div)

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Model.ActiveMode (ActiveMode(..))
import Specular.Dom.Browser (Attrs)
import Specular.Dom.Element (attrsD, class_, classes, el, text)
import Specular.Dom.Widget (Widget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (Dynamic, Event, filterEvent, holdDyn, leftmost, newEvent, subscribeEvent_)
import UI.Utils (div, mkAttrs, mkStyle, (:~))

data ConfirmResult = Confirmed
                   | Cancelled

derive instance genericConfirmResult :: Generic ConfirmResult _
derive instance eqConfirmResult :: Eq ConfirmResult
instance showConfirmResult :: Show ConfirmResult where
    show = genericShow

dialogAttr :: ActiveMode -> Attrs
dialogAttr m = mkStyle ["position"          :~ "absolute",
                         "top"              :~ "40%",
                         "left"             :~ "50%",
                         "width"            :~ "500px",
                         "height"           :~ "120px",
                         "margin-left"      :~ "-250px",
                         "margin-top"       :~ "-60px",
                         "z-index"          :~ "2500",
                         "background-color" :~ "white",
                         "border-radius"    :~ "5px",
                         "pointer-events"   :~ "auto",
                         "display"          :~ display m
                        ]
    where display Active   = "block"
          display Inactive = "none"

confirmDialog :: Dynamic ActiveMode -> Widget Unit -> Widget (Event ConfirmResult)
confirmDialog modeDyn child =
    div [attrsD $ dialogAttr <$> modeDyn,
         classes ["uk-box-shadow-medium"]] $
        el "form" [] do
            div [class_ "uk-modal-body"] child
            div [classes ["uk-modal-footer", "uk-text-center"]] do
                -- make sure the button has "type = 'button'" attribute, or else
                -- it will be used as "submit" inside form and redirect
                confirmE <- buttonOnClick (pure $ mkAttrs ["class" :~ "uk-button uk-button-primary",
                                                           "type" :~ "button"]) $ text "Continue"
                cancelE  <- buttonOnClick (pure $ mkAttrs ["class" :~ "uk-button uk-button-default",
                                                           "type" :~ "button"]) $ text "Cancel"
                pure $ leftmost [const Confirmed <$> confirmE,
                                 const Cancelled <$> cancelE]

-- show confirm dialog to let user confirm an action
askConfirm :: Event Unit -> Widget (Event Unit)
askConfirm evt = do
    { event: closeEvt, fire: toClose } <- newEvent

    actDyn <- holdDyn Inactive $ leftmost [const Active <$> evt,
                                           const Inactive <$> closeEvt]
    e <- confirmDialog actDyn (text "A.I. will redesign the solar system when roof plates are edited")
    subscribeEvent_ toClose e

    pure $ const unit <$> filterEvent ((==) Confirmed) e
