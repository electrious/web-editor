module SmartHouse.ActiveItemUI where

import Prelude hiding (div, degree)

import Control.Alternative (empty)
import Data.Compactable (compact)
import Data.Default (class Default, def)
import Data.Lens (Lens', view, (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_roof, _shade, _shadeSelected, _slope, _slopeSelected)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, dynEvent, latestEvt)
import FRP.Event (Event)
import Math.Angle (Angle, degreeVal, fromString)
import Model.SmartHouse.Roof (Roof)
import Models.SmartHouse.ActiveItem (ActiveItem(..), activeRoof)
import SmartHouse.ShadeOption (ShadeOption)
import SmartHouse.ShadeOptionUI (shadeSelector)
import Specular.Dom.Browser (Attrs)
import Specular.Dom.Element (attr, attrsD, bindValueOnChange, bindValueOnInput, class_, classes, dynText, valueD)
import Specular.Dom.Element.Class (el)
import Specular.Dom.Widget (Widget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (dynamic)
import Specular.Ref (new, value)
import Type.Proxy (Proxy(..))
import UI.Bridge (fromUIDyn, fromUIEvent, toUIDyn)
import UI.Utils (div, mkAttrs, mkStyle, (:~))


newtype ActiveItemUI = ActiveItemUI {
    deleteHouse   :: Event Unit,
    slopeSelected :: Event Angle,
    shadeSelected :: Event ShadeOption
    }

derive instance Newtype ActiveItemUI _
instance Default ActiveItemUI where
    def = ActiveItemUI {
        deleteHouse   : empty,
        slopeSelected : empty,
        shadeSelected : empty
        }

_deleteHouse :: forall t a r. Newtype t { deleteHouse :: a | r } => Lens' t a
_deleteHouse = _Newtype <<< prop (Proxy :: Proxy "deleteHouse")



activeItemUIStyle :: Boolean -> Attrs
activeItemUIStyle d = mkStyle [
    "display" :~ if d then "flex" else "none"
    ]


getShadeOption :: ActiveItem -> Maybe ShadeOption
getShadeOption (ActiveHouse r) = view _shade <$> r ^. _roof
getShadeOption (ActiveTree _) = Nothing


subtitle :: Maybe ActiveItem -> String
subtitle (Just (ActiveHouse _)) = "Current House:"
subtitle (Just (ActiveTree _)) = "Current Tree:"
subtitle Nothing = ""

delBtnLabel :: Maybe ActiveItem -> String
delBtnLabel (Just (ActiveHouse _)) = "Delete This House"
delBtnLabel (Just (ActiveTree _)) = "Delete This Tree"
delBtnLabel Nothing = ""


delButton :: Dynamic (Maybe ActiveItem) -> Widget (Event Unit)
delButton actItemDyn =
    div [classes ["uk-flex", "uk-flex-column", "uk-margin-top"]] do
        d <- liftEffect $ toUIDyn actItemDyn
        div [class_ "uk-text-bold"] $ dynText $ subtitle <$> d
        
        e <- buttonOnClick (pure $ mkAttrs ["class" :~ "uk-button uk-button-danger"]) $ dynText $ delBtnLabel <$> d
        fromUIEvent e

houseSlopeUI :: Angle -> Widget (Event Angle)
houseSlopeUI slope =
    div [classes ["uk-flex"]] do
        let slopeStr = show $ degreeVal slope
        -- slope ref bind to the change value for each interaction
        slopeRef    <- new slopeStr

        -- slope ref bind to the change only when the value changes
        slopeValRef <- new slopeStr

        -- show the range slide to select the slope
        el "input" [attr "type" "range",
                    attr "min" "5",
                    attr "max" "85",
                    attr "step" "1",
                    valueD $ pure slopeStr,
                    bindValueOnInput slopeRef,
                    bindValueOnChange slopeValRef
                    ] $ pure unit

        let newSlopeDyn = value slopeRef
            newSlopeStrDyn = (flip (<>) "°") <$> newSlopeDyn

        el "div" [ class_ "uk-margin-left"] $ dynText $ newSlopeStrDyn

        slopeValDyn <- fromUIDyn $ fromString <$> value slopeRef

        pure $ compact $ dynEvent slopeValDyn


dynSlopeUI :: Dynamic (Maybe Roof) -> Widget (Event Angle)
dynSlopeUI roofDyn = do
    let f (Just r) = houseSlopeUI $ r ^. _slope
        f Nothing  = pure empty
    
    rDyn <- liftEffect $ toUIDyn roofDyn
    resDyn <- fromUIDyn =<< dynamic (f <$> rDyn)
    pure $ latestEvt resDyn

activeItemUI :: Dynamic (Maybe ActiveItem) -> Widget ActiveItemUI
activeItemUI actItemDyn = do
    styleD <- liftEffect $ toUIDyn $ activeItemUIStyle <<< isJust <$> actItemDyn
    div [classes ["uk-flex", "uk-flex-column", "uk-margin-top"],
         attrsD styleD] do
        
        slopeEvt <- dynSlopeUI $ join <<< map activeRoof <$> actItemDyn

        selEvt <- shadeSelector $ join <<< map getShadeOption <$> actItemDyn
        delEvt <- delButton actItemDyn

        pure $ def # _shadeSelected .~ selEvt
                   # _slopeSelected .~ slopeEvt
                   # _deleteHouse   .~ delEvt
