module SmartHouse.ActiveItemUI where

import Prelude hiding (div, degree)

import Control.Alternative (empty)
import Data.Default (class Default, def)
import Data.Int (round)
import Data.Lens (Lens', view, (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_roof, _shade, _shadeSelected, _slope, _slopeSelected)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic)
import FRP.Event (Event, create)
import Math.Angle (Angle, degreeVal, fromString)
import Model.SmartHouse.Roof (Roof)
import Models.SmartHouse.ActiveItem (ActiveItem(..), activeRoof)
import SmartHouse.ShadeOption (ShadeOption)
import SmartHouse.ShadeOptionUI (shadeSelector)
import Specular.Dom.Browser (Attrs)
import Specular.Dom.Browser as DOM
import Specular.Dom.Element (attr, attrsD, class_, classes, dynText, on, valueD)
import Specular.Dom.Element.Class (el)
import Specular.Dom.Node.Class (Node)
import Specular.Dom.Widget (Widget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.Dom.Widgets.Input (getTextInputValue)
import Type.Proxy (Proxy(..))
import UI.Bridge (fromUIEvent, toUIDyn)
import UI.Utils (div, mkAttrs, mkStyle, visible, (:~))
import Unsafe.Coerce (unsafeCoerce)


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

showAngle :: Angle -> String
showAngle a = show (round $ degreeVal a)

showMaybeAngle :: Maybe Angle -> String
showMaybeAngle Nothing  = "0°"
showMaybeAngle (Just a) = showAngle a

appendDegSym :: String -> String
appendDegSym s = s <> "°"

withTargetValue :: (String -> Effect Unit) -> (DOM.Event -> Effect Unit)
withTargetValue cb = \event -> do
  value <- getTextInputValue (unsafeEventTarget event)
  cb value

unsafeEventTarget :: DOM.Event -> Node
unsafeEventTarget e = (unsafeCoerce e).target

houseSlopeUI :: Dynamic (Maybe Angle) -> Widget (Event Angle)
houseSlopeUI slopeDyn = do
    slopeDynU <- liftEffect $ toUIDyn slopeDyn
    div [class_ "uk-flex", visible (isJust <$> slopeDynU)] do
        let slopeStrDyn = showMaybeAngle <$> slopeDynU

        -- create new slope event for user interaction
        { event: slopeEvt, push: slopePush } <- liftEffect create
        -- convert value string to slope angle and push it to the event
        let pushNewVal v = case fromString v of
                Just a -> slopePush a
                Nothing -> pure unit

        -- show the range slide to select the slope
        el "input" [attr "type" "range",
                    attr "min" "5",
                    attr "max" "85",
                    attr "step" "1",
                    valueD slopeStrDyn,
                    on "input" (withTargetValue pushNewVal)
                    ] $ pure unit

        el "div" [ class_ "uk-margin-left"] $ dynText $ appendDegSym <$> slopeStrDyn

        pure slopeEvt


dynSlopeUI :: Dynamic (Maybe Roof) -> Widget (Event Angle)
dynSlopeUI roofDyn = houseSlopeUI (map (view _slope) <$> roofDyn)

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
