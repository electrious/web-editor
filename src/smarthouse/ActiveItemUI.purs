module SmartHouse.ActiveItemUI where

import Prelude hiding (div, degree)

import Control.Alternative (empty)
import Data.Compactable (compact)
import Data.Default (class Default, def)
import Data.Int (round)
import Data.Lens (view, (.~), (^.))
import Data.List (head)
import Data.Map (values)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Data.UUIDWrapper (UUID)
import Editor.Common.Lenses (_buildChimney, _chimney, _delChimney, _deleted, _roof, _roofs, _slope, _slopeSelected)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, dynEvent, sampleDyn_, step)
import FRP.Event (Event, create)
import Foreign.Object as Obj
import Math.Angle (Angle, degree, degreeVal, fromString)
import Model.SmartHouse.Chimney (Chimney)
import Model.SmartHouse.House (defaultSlope)
import Model.UUID (idLens)
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
import Specular.FRP (attachDynWith, fixEvent, holdDyn, weaken)
import Specular.FRP as S
import UI.Bridge (fromUIDyn, fromUIEvent, toUIDyn)
import UI.Utils (div, mkAttrs, mkStyle, visible, (:~))
import Unsafe.Coerce (unsafeCoerce)


newtype ActiveItemUI = ActiveItemUI {
    deleted       :: Event Unit,
    slopeSelected :: Event SlopeOption,
    buildChimney  :: Event Boolean,
    delChimney    :: Event UUID
    }

derive instance Newtype ActiveItemUI _
instance Default ActiveItemUI where
    def = ActiveItemUI {
        deleted       : empty,
        slopeSelected : empty,
        buildChimney  : empty,
        delChimney    : empty
        }

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

appendDegSym :: String -> String
appendDegSym s = s <> "°"

withTargetValue :: (String -> Effect Unit) -> (DOM.Event -> Effect Unit)
withTargetValue cb = \event -> do
  value <- getTextInputValue (unsafeEventTarget event)
  cb value

unsafeEventTarget :: DOM.Event -> Node
unsafeEventTarget e = (unsafeCoerce e).target

{-
--------------------------------------------
-- Slope UI
--------------------------------------------
-}

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

houseSlopeUI :: Dynamic Angle -> Widget (Event SlopeOption)
houseSlopeUI slopeDyn = do
    text "Slope:"
    slopeEvt <- slopeSelector slopeDyn
    scopeD   <- slopeScopeUI
    
    let slopeD = step (degree 30.0) slopeEvt
    pure $ dynEvent $ slopeOption <$> slopeD <*> scopeD

{-
--------------------------------------------
-- Chimney UI
--------------------------------------------
-}
newtype ChimneyUIEvts = ChimneyUIEvts {
    buildChimney :: Event Boolean,
    delChimney   :: Event UUID
}

derive instance Newtype ChimneyUIEvts _
instance Default ChimneyUIEvts where
    def = ChimneyUIEvts {
        buildChimney : empty,
        delChimney   : empty
    }

-- | button to allow user to build a new chimney
chimneyBtn :: Widget (S.Event Boolean)
chimneyBtn = toggleBtn label Nothing
    where label true  = "Stop Building Chimney"
          label false = "Build a Chimney"

-- | button to allow user to toggle a state
toggleBtn :: (Boolean -> String) -> Maybe String -> Widget (S.Event Boolean)
toggleBtn label cls = fixEvent \tapEvt -> do
    d <- holdDyn false tapEvt
    e <- buttonOnClick (pure $ mkAttrs ["class" :~ ("uk-button " <> fromMaybe "" cls)]) (dynText $ label <$> d)
    let nextEvt = attachDynWith (const <<< not) d e
    pure $ Tuple nextEvt nextEvt


displayStyle :: Boolean -> String
displayStyle true = "block"
displayStyle false = "none"

-- | delete active chimney button
delChimneyBtn :: Dynamic Boolean -> Widget (Event Unit)
delChimneyBtn showDyn = do
    let mkAttr s = mkAttrs ["class" :~ "uk-button uk-button-danger",
                            "style" :~ ("display: " <> displayStyle s)]

    attD <- liftEffect $ toUIDyn $ mkAttr <$> showDyn
    e <- buttonOnClick (weaken attD) $ text "Delete this Chimney"
    fromUIEvent e

chimneyUI :: Dynamic (Maybe Chimney) -> Widget ChimneyUIEvts
chimneyUI actChimDyn = div [classes ["uk-flex", "uk-flex-column", "uk-margin-top"]] do
    text "Chimney:"
    -- button to toggle building chimney mode
    chimModeEvt <- fromUIEvent =<< chimneyBtn

    -- button to delete active chimney
    delEvt <- delChimneyBtn (isJust <$> actChimDyn)

    let delChimEvt = compact $ sampleDyn_ actChimDyn delEvt

    pure $ def # _buildChimney .~ chimModeEvt
               # _delChimney   .~ (view idLens <$> delChimEvt)

{-
--------------------------------------------
-- Active house UI
--------------------------------------------
-}
newtype ActHouseEvts = ActHouseEvts {
    slope   :: Event SlopeOption,
    chimney :: ChimneyUIEvts
}

derive instance Newtype ActHouseEvts _
instance Default ActHouseEvts where
    def = ActHouseEvts {
        slope   : empty,
        chimney : def
    }

houseActiveUI :: Dynamic Boolean -> Dynamic (Maybe ActiveItem) -> Widget ActHouseEvts
houseActiveUI showDyn actItemDyn = do
    visDyn   <- liftEffect $ toUIDyn showDyn
    let slopeDyn = getSlope <<< join <<< map activeHouse <$> actItemDyn
        chimDyn  = join <<< map (view _chimney) <<< join <<< map activeHouse <$> actItemDyn
    div [classes ["uk-flex", "uk-flex-column", "uk-margin-top"], visible visDyn] do
        slopeEvt <- houseSlopeUI slopeDyn
        chimEvts <- chimneyUI chimDyn

        pure $ def # _slope   .~ slopeEvt
                   # _chimney .~ chimEvts

activeItemUI :: Dynamic (Maybe ActiveItem) -> Widget ActiveItemUI
activeItemUI actItemDyn = do
    styleD <- liftEffect $ toUIDyn $ activeItemUIStyle <<< isJust <$> actItemDyn
    div [classes ["uk-flex", "uk-flex-column", "uk-margin-top"],
         attrsD styleD] do
        -- subtitle
        d <- liftEffect $ toUIDyn actItemDyn
        div [class_ "uk-text-bold"] $ dynText $ subtitle <$> d

        -- slope
        let isActHouseDyn = maybe false isActiveHouse <$> actItemDyn
        hEvts <- houseActiveUI isActHouseDyn actItemDyn

        -- delete button
        delEvt <- delButton actItemDyn

        pure $ def # _slopeSelected .~ (hEvts ^. _slope)
                   # _deleted       .~ delEvt
                   # _buildChimney  .~ (hEvts ^. _chimney <<< _buildChimney)
                   # _delChimney    .~ (hEvts ^. _chimney <<< _delChimney)
