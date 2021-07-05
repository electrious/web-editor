module SmartHouse.ShadeOptionUI where

import Prelude hiding (div)

import Data.Maybe (Maybe, fromMaybe, isJust)
import Data.Traversable (class Traversable, traverse)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, gateDyn)
import FRP.Event (Event)
import FRP.Event.Extra (anyEvt)
import Model.ActiveMode (ActiveMode(..), fromBoolean, isActive)
import SmartHouse.ShadeOption (ShadeOption(..))
import Specular.Dom.Browser (Attrs, (:=))
import Specular.Dom.Builder.Class (text)
import Specular.Dom.Element (attrsD, class_, classes)
import Specular.Dom.Widget (Widget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (weaken)
import UI.Bridge (fromUIEvent, toUIDyn)
import UI.Utils (div, mkStyle, (:~))


-- a clickable selector option used in a selector list
selectorOption :: forall a. Show a => a -> Dynamic ActiveMode -> Widget (Event a)
selectorOption v actDyn = do
    let mkAtt Active   = "class" := "uk-button uk-button-primary"
        mkAtt Inactive = "class" := "uk-button uk-button-default uk-hide"

    attD <- liftEffect $ toUIDyn (mkAtt <$> actDyn)
    e <- fromUIEvent =<< buttonOnClick (weaken attD) (text $ show v)

    -- only inactive button can be tapped
    pure $ gateDyn (not <<< isActive <$> actDyn) $ const v <$> e


-- a list selector to select an option from a list of candidates.
-- actDyn denotes the currently selected option
selectList :: forall a f. Traversable f => Eq a => Show a => f a -> Dynamic a -> Widget (Event a)
selectList vs actDyn = div [classes ["uk-button-group", "uk-flex", "uk-flex-column"]] $
                           anyEvt <$> traverse f vs
    where f v = selectorOption v (fromBoolean <<< (==) v <$> actDyn)


shadeSelectorStyle :: Boolean -> Attrs
shadeSelectorStyle d = mkStyle [
    "display" :~ if d then "flex" else "none"
    ]

shadeSelector :: Dynamic (Maybe ShadeOption) -> Widget (Event ShadeOption)
shadeSelector shadeDyn = do
    styleD <- liftEffect $ toUIDyn $ shadeSelectorStyle <<< isJust <$> shadeDyn
    div [classes ["uk-flex", "uk-flex-column"],
         attrsD styleD] do
        div [class_ "uk-text-bold"] $ text "Select Shading:"

        let actDyn = fromMaybe NoShade <$> shadeDyn
        selectList [NoShade, LightShade, ModerateShade, HeavyShade] actDyn
