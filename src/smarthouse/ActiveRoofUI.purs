module SmartHouse.ActiveRoofUI where

import Prelude hiding (div)

import Control.Alternative (empty)
import Data.Default (class Default, def)
import Data.Lens (Lens', view, (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe, isJust)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_shade, _shadeSelected)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic)
import FRP.Event (Event)
import Model.SmartHouse.Roof (Roof)
import SmartHouse.ShadeOption (ShadeOption)
import SmartHouse.ShadeOptionUI (shadeSelector)
import Specular.Dom.Browser (Attrs)
import Specular.Dom.Builder.Class (text)
import Specular.Dom.Element (attrsD, class_, classes)
import Specular.Dom.Widget (Widget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import UI.Bridge (fromUIEvent, toUIDyn)
import UI.Utils (div, mkAttrs, mkStyle, (:~))


newtype ActiveRoofUI = ActiveRoofUI {
    deleteHouse   :: Event Unit,
    shadeSelected :: Event ShadeOption
    }

derive instance newtypeActiveRoofUI :: Newtype ActiveRoofUI _
instance defaultActiveRoofUI :: Default ActiveRoofUI where
    def = ActiveRoofUI {
        deleteHouse   : empty,
        shadeSelected : empty
        }

_deleteHouse :: forall t a r. Newtype t { deleteHouse :: a | r } => Lens' t a
_deleteHouse = _Newtype <<< prop (SProxy :: SProxy "deleteHouse")



activeRoofUIStyle :: Boolean -> Attrs
activeRoofUIStyle d = mkStyle [
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


delButton :: Widget (Event Unit)
delButton =
    div [classes ["uk-flex", "uk-flex-column", "uk-margin-top"]] do
        div [class_ "uk-text-bold"] $ text "Current House:"
        
        e <- buttonOnClick (pure $ mkAttrs ["class" :~ "uk-button uk-button-danger"]) $ text "Delete This House"
        fromUIEvent e

activeRoofUI :: Dynamic (Maybe Roof) -> Widget ActiveRoofUI
activeRoofUI actRoofDyn = do
    styleD <- liftEffect $ toUIDyn $ activeRoofUIStyle <<< isJust <$> actRoofDyn
    div [classes ["uk-flex", "uk-flex-column"],
         attrsD styleD] do
        selEvt <- shadeSelector $ map (view _shade) <$> actRoofDyn
        delEvt <- delButton

        pure $ def # _shadeSelected .~ selEvt
                   # _deleteHouse   .~ delEvt
