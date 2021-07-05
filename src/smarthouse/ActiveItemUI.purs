module SmartHouse.ActiveItemUI where

import Prelude hiding (div)

import Control.Alternative (empty)
import Data.Default (class Default, def)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_shade, _shadeSelected)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic)
import FRP.Event (Event)
import Models.SmartHouse.ActiveItem (ActiveItem(..))
import SmartHouse.ShadeOption (ShadeOption)
import SmartHouse.ShadeOptionUI (shadeSelector)
import Specular.Dom.Browser (Attrs)
import Specular.Dom.Element (attrsD, class_, classes, dynText)
import Specular.Dom.Widget (Widget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import UI.Bridge (fromUIEvent, toUIDyn)
import UI.Utils (div, mkAttrs, mkStyle, (:~))


newtype ActiveItemUI = ActiveItemUI {
    deleteHouse   :: Event Unit,
    shadeSelected :: Event ShadeOption
    }

derive instance newtypeActiveItemUI :: Newtype ActiveItemUI _
instance defaultActiveItemUI :: Default ActiveItemUI where
    def = ActiveItemUI {
        deleteHouse   : empty,
        shadeSelected : empty
        }

_deleteHouse :: forall t a r. Newtype t { deleteHouse :: a | r } => Lens' t a
_deleteHouse = _Newtype <<< prop (SProxy :: SProxy "deleteHouse")



activeItemUIStyle :: Boolean -> Attrs
activeItemUIStyle d = mkStyle [
    "display" :~ if d then "flex" else "none"
    ]


getShadeOption :: ActiveItem -> Maybe ShadeOption
getShadeOption (ActiveRoof r) = Just $ r ^. _shade
getShadeOption (ActiveTree _) = Nothing


subtitle :: Maybe ActiveItem -> String
subtitle (Just (ActiveRoof _)) = "Current House:"
subtitle (Just (ActiveTree _)) = "Current Tree:"
subtitle Nothing = ""

delBtnLabel :: Maybe ActiveItem -> String
delBtnLabel (Just (ActiveRoof _)) = "Delete This House"
delBtnLabel (Just (ActiveTree _)) = "Delete This Tree"
delBtnLabel Nothing = ""


delButton :: Dynamic (Maybe ActiveItem) -> Widget (Event Unit)
delButton actItemDyn =
    div [classes ["uk-flex", "uk-flex-column", "uk-margin-top"]] do
        d <- liftEffect $ toUIDyn actItemDyn
        div [class_ "uk-text-bold"] $ dynText $ subtitle <$> d
        
        e <- buttonOnClick (pure $ mkAttrs ["class" :~ "uk-button uk-button-danger"]) $ dynText $ delBtnLabel <$> d
        fromUIEvent e

activeItemUI :: Dynamic (Maybe ActiveItem) -> Widget ActiveItemUI
activeItemUI actItemDyn = do
    styleD <- liftEffect $ toUIDyn $ activeItemUIStyle <<< isJust <$> actItemDyn
    div [classes ["uk-flex", "uk-flex-column", "uk-margin-top"],
         attrsD styleD] do
        selEvt <- shadeSelector $ join <<< map getShadeOption <$> actItemDyn
        delEvt <- delButton actItemDyn

        pure $ def # _shadeSelected .~ selEvt
                   # _deleteHouse   .~ delEvt
