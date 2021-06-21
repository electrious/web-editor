module SmartHouse.ShadeOptionUI where

import Prelude hiding (div)

import Data.Maybe (Maybe, fromMaybe, isJust)
import Data.Traversable (class Traversable, traverse)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, debugDyn, gateDyn)
import FRP.Event (Event)
import FRP.Event.Extra (anyEvt)
import Foreign.Object (singleton)
import Model.ActiveMode (ActiveMode(..), fromBoolean, isActive)
import SmartHouse.ShadeOption (ShadeOption(..))
import Specular.Dom.Browser (Attrs, (:=))
import Specular.Dom.Builder.Class (text)
import Specular.Dom.Element (attrD, attrs, attrsD, class_, classes)
import Specular.Dom.Widget (Widget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (weaken)
import UI.Bridge (fromUIEvent, toUIDyn)
import UI.Utils (div, mkStyle, (:~))


-- a clickable selector option used in a selector list
selectorOption :: forall a. Show a => a -> Dynamic ActiveMode -> Widget (Event a)
selectorOption v actDyn = do
    let mkAtt Active   = "class" := "uk-button uk-button-primary"
        mkAtt Inactive = "class" := "uk-button uk-button-default"

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


selectorStyle :: Boolean -> Attrs
selectorStyle d = mkStyle [
    "position"       :~ "absolute",
    "background"     :~ "white",
    "width"          :~ "220px",
    "top"            :~ "80px",
    "right"          :~ "20px",
    "padding"        :~ "5px",
    "z-index"        :~ "10",
    "pointer-events" :~ "auto",
    "display"        :~ if d then "flex" else "none"
    ]


shadeSelector :: Dynamic (Maybe ShadeOption) -> Widget (Event ShadeOption)
shadeSelector shadeDyn = do
    styleD <- liftEffect $ toUIDyn $ selectorStyle <<< isJust <$> debugDyn shadeDyn
    div [classes ["uk-flex", "uk-flex-column"],
         attrsD styleD] do
        div [class_ "uk-text-bold"] $ text "Select Shading:"

        let actDyn = fromMaybe NoShade <$> shadeDyn
        selectList [NoShade, LightShade, ModerateShade, HeavyShade] actDyn
