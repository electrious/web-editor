module SmartHouse.ActiveItemUI where

import Prelude hiding (div, degree)

import Control.Alternative (empty)
import Data.Default (class Default, def)
import Data.Int (round)
import Data.Lens (view, (.~), (^.))
import Data.List (head)
import Data.Map (values)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_chimney, _deleted, _roof, _roofs, _slope, _slopeSelected)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, dynEvent, step)
import FRP.Event (Event, create)
import Foreign.Object as Obj
import Math.Angle (Angle, degree, degreeVal, fromString)
import Model.SmartHouse.House (defaultSlope)
import Models.SmartHouse.ActiveItem (ActHouseItem, ActiveItem(..), activeHouse, isActiveHouse)
import SmartHouse.HouseEditor (_house)
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
import UI.Bridge (fromUIDyn, fromUIEvent, toUIDyn)
import UI.Utils (div, mkAttrs, mkStyle, visible, (:~))
import Unsafe.Coerce (unsafeCoerce)


newtype ActiveItemUI = ActiveItemUI {
    deleted       :: Event Unit,
    slopeSelected :: Event SlopeOption
    }

derive instance Newtype ActiveItemUI _
instance Default ActiveItemUI where
    def = ActiveItemUI {
        deleted       : empty,
        slopeSelected : empty
        }

activeItemUIStyle :: Boolean -> Attrs
activeItemUIStyle d = mkStyle [
    "display" :~ if d then "flex" else "none"
    ]


subtitle :: Maybe ActiveItem -> String
subtitle (Just (ActiveHouse h)) = case h ^. _chimney of
    Nothing -> "Current House:"
    Just _  -> "Current Chimney:"
subtitle (Just (ActiveTree _)) = "Current Tree:"
subtitle Nothing = ""

delBtnLabel :: Maybe ActiveItem -> String
delBtnLabel (Just (ActiveHouse h)) = case h ^. _chimney of
    Nothing -> "Delete This House"
    Just _  -> "Delete This Chimney"
delBtnLabel (Just (ActiveTree _)) = "Delete This Tree"
delBtnLabel Nothing = ""


delButton :: Dynamic (Maybe ActiveItem) -> Widget (Event Unit)
delButton actItemDyn = do
    d <- liftEffect $ toUIDyn actItemDyn
    e <- buttonOnClick (pure $ mkAttrs ["class" :~ "uk-button uk-button-danger uk-margin-top"]) $ dynText $ delBtnLabel <$> d
    fromUIEvent e

showAngle :: Angle -> String
showAngle a = show (round $ degreeVal a)

appendDegSym :: String -> String
appendDegSym s = s <> "°"

withTargetValue :: (String -> Effect Unit) -> (DOM.Event -> Effect Unit)
withTargetValue cb = \event -> do
  value <- getTextInputValue (unsafeEventTarget event)
  cb value

unsafeEventTarget :: DOM.Event -> Node
unsafeEventTarget e = (unsafeCoerce e).target

slopeTop :: Angle
slopeTop = degree 89.6

-- make sure slope is at most 89.6° for gable roof
safeSlope :: Angle -> Angle
safeSlope a = if a > slopeTop then slopeTop else a

slopeSelector :: Dynamic Angle -> Widget (Event Angle)
slopeSelector slopeDyn = div [class_ "uk-flex"] do
    slopeStrDyn <- liftEffect $ toUIDyn $ showAngle <$> slopeDyn

    -- create new slope event for user interaction
    { event: slopeEvt, push: slopePush } <- liftEffect create
    -- convert value string to slope angle and push it to the event
    let pushNewVal v = case fromString v of
            Just a -> slopePush a
            Nothing -> pure unit

    -- show the range slide to select the slope
    el "input" [attr "type" "range",
                attr "min" "5",
                attr "max" "90",
                attr "step" "1",
                valueD slopeStrDyn,
                on "input" (withTargetValue pushNewVal)
                ] $ pure unit

    el "div" [ class_ "uk-margin-left"] $ dynText $ appendDegSym <$> slopeStrDyn
    pure $ safeSlope <$> slopeEvt


slopeScopeUI :: Widget (Dynamic Boolean)
slopeScopeUI = div [classes ["uk-flex", "uk-flex-row", "uk-flex-middle"]] do
    checkD <- checkbox true Obj.empty
    text "Apply to All Roofs"

    fromUIDyn checkD

getSlope :: Maybe ActHouseItem -> Angle
getSlope (Just h) = case h ^. _roof of
    Just r  -> r ^. _slope
    Nothing -> let r = head $ values $ h ^. _house <<< _roofs
               in maybe defaultSlope (view _slope) r
getSlope Nothing  = defaultSlope

houseSlopeUI :: Dynamic Boolean -> Dynamic Angle -> Widget (Event SlopeOption)
houseSlopeUI showDyn slopeDyn = do
    visDyn   <- liftEffect $ toUIDyn showDyn
    div [classes ["uk-flex", "uk-flex-column", "uk-margin-top"], visible visDyn] do
        text "Slope:"
        slopeEvt <- slopeSelector slopeDyn
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
        slopeEvt <- houseSlopeUI isActHouseDyn $ getSlope <<< join <<< map activeHouse <$> actItemDyn

        delEvt <- delButton actItemDyn

        pure $ def # _slopeSelected .~ slopeEvt
                   # _deleted       .~ delEvt
