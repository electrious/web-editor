module SmartHouse.ActiveItemUI where

import Prelude hiding (div, degree)

import Control.Alternative (empty)
import Data.Default (class Default, def)
import Data.Int (round)
import Data.Lens (Lens', view, (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_slope, _slopeSelected)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, dynEvent, step)
import FRP.Event (Event, create)
import Foreign.Object as Obj
import Math.Angle (Angle, degree, degreeVal, fromString)
import Model.SmartHouse.Roof (Roof)
import Models.SmartHouse.ActiveItem (ActiveItem(..), activeRoof, isActiveHouse)
import SmartHouse.SlopeOption (SlopeOption, slopeOption)
import Specular.Dom.Browser (Attrs)
import Specular.Dom.Browser as DOM
import Specular.Dom.Builder.Class (text)
import Specular.Dom.Element (attr, attrsD, class_, classes, dynText, on, valueD)
import Specular.Dom.Element.Class (el)
import Specular.Dom.Node.Class (Node)
import Specular.Dom.Widget (Widget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.Dom.Widgets.Input (checkbox, getTextInputValue)
import Type.Proxy (Proxy(..))
import UI.Bridge (fromUIDyn, fromUIEvent, toUIDyn)
import UI.Utils (div, mkAttrs, mkStyle, visible, (:~))
import Unsafe.Coerce (unsafeCoerce)


newtype ActiveItemUI = ActiveItemUI {
    deleteHouse   :: Event Unit,
    slopeSelected :: Event SlopeOption
    }

derive instance Newtype ActiveItemUI _
instance Default ActiveItemUI where
    def = ActiveItemUI {
        deleteHouse   : empty,
        slopeSelected : empty
        }

_deleteHouse :: forall t a r. Newtype t { deleteHouse :: a | r } => Lens' t a
_deleteHouse = _Newtype <<< prop (Proxy :: Proxy "deleteHouse")

activeItemUIStyle :: Boolean -> Attrs
activeItemUIStyle d = mkStyle [
    "display" :~ if d then "flex" else "none"
    ]


subtitle :: Maybe ActiveItem -> String
subtitle (Just (ActiveHouse _)) = "Current House:"
subtitle (Just (ActiveTree _)) = "Current Tree:"
subtitle Nothing = ""

delBtnLabel :: Maybe ActiveItem -> String
delBtnLabel (Just (ActiveHouse _)) = "Delete This House"
delBtnLabel (Just (ActiveTree _)) = "Delete This Tree"
delBtnLabel Nothing = ""


delButton :: Dynamic (Maybe ActiveItem) -> Widget (Event Unit)
delButton actItemDyn = do
    d <- liftEffect $ toUIDyn actItemDyn
    e <- buttonOnClick (pure $ mkAttrs ["class" :~ "uk-button uk-button-danger uk-margin-top"]) $ dynText $ delBtnLabel <$> d
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


slopeSelector :: Dynamic (Maybe Angle) -> Widget (Event Angle)
slopeSelector slopeDyn = div [class_ "uk-flex"] do
    slopeStrDyn <- liftEffect $ toUIDyn $ showMaybeAngle <$> slopeDyn

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


slopeScopeUI :: Widget (Dynamic Boolean)
slopeScopeUI = div [classes ["uk-flex", "uk-flex-row", "uk-flex-middle"]] do
    checkD <- checkbox true Obj.empty
    text "Apply to All Roofs"

    fromUIDyn checkD

houseSlopeUI :: Dynamic Boolean -> Dynamic (Maybe Roof) -> Widget (Event SlopeOption)
houseSlopeUI showDyn roofDyn = do
    visDyn   <- liftEffect $ toUIDyn showDyn
    div [classes ["uk-flex", "uk-flex-column", "uk-margin-top"], visible visDyn] do
        text "Slope:"
        slopeEvt <- slopeSelector $ map (view _slope) <$> roofDyn
        scopeD   <- slopeScopeUI
        
        let slopeD = step (degree 30.0) slopeEvt
        pure $ dynEvent $ slopeOption <$> slopeD <*> scopeD

activeItemUI :: Dynamic (Maybe ActiveItem) -> Widget ActiveItemUI
activeItemUI actItemDyn = do
    styleD <- liftEffect $ toUIDyn $ activeItemUIStyle <<< isJust <$> actItemDyn
    div [classes ["uk-flex", "uk-flex-column", "uk-margin-top"],
         attrsD styleD] do
        -- subtitle
        d <- liftEffect $ toUIDyn actItemDyn
        div [class_ "uk-text-bold"] $ dynText $ subtitle <$> d

        let isActHouseDyn = maybe false isActiveHouse <$> actItemDyn
        slopeEvt <- houseSlopeUI isActHouseDyn $ join <<< map activeRoof <$> actItemDyn

        delEvt <- delButton actItemDyn

        pure $ def # _slopeSelected .~ slopeEvt
                   # _deleteHouse   .~ delEvt
