module SmartHouse.UI where

import Prelude hiding (div)

import API.SmartHouse (SavingStep(..), stepMode)
import Control.Plus (empty)
import Data.Default (class Default, def)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_buildChimney, _buildTree, _buttons, _deleted, _height, _slopeSelected, _width)
import Editor.Editor (_sizeDyn)
import Editor.SceneEvent (Size, size)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic)
import FRP.Event (Event)
import Models.SmartHouse.ActiveItem (ActiveItem)
import SmartHouse.SlopeOption (SlopeOption)
import Specular.Dom.Element (attrsD, class_, classes, dynText, el)
import Specular.Dom.Widget (Widget)
import Specular.FRP as S
import Type.Proxy (Proxy(..))
import UI.Bridge (toUIDyn)
import UI.ButtonPane (ButtonsPane, _showCloseDyn, _showResetDyn, _showSaveDyn, _showUndoDyn, buttons)
import UI.ConfirmDialog (dialogAttr)
import UI.EditPane (_activeItem, editPane)
import UI.Utils (div, mkStyle, (:~))

newtype BuilderUIConf = BuilderUIConf {
    sizeDyn       :: Dynamic Size,
    showSaveDyn   :: Dynamic Boolean,
    showResetDyn  :: Dynamic Boolean,
    savingStepDyn :: Dynamic SavingStep,
    activeItemDyn :: Dynamic (Maybe ActiveItem)
    }

derive instance newtypeBuilderUIConf :: Newtype BuilderUIConf _
instance defaultBuilderUIConf :: Default BuilderUIConf where
    def = BuilderUIConf {
        sizeDyn       : pure (size 10 10),
        showSaveDyn   : pure false,
        showResetDyn  : pure false,
        savingStepDyn : pure NotSaving,
        activeItemDyn : pure Nothing
        }

_savingStepDyn :: forall t a r. Newtype t { savingStepDyn :: a | r } => Lens' t a
_savingStepDyn = _Newtype <<< prop (Proxy :: Proxy "savingStepDyn")

_activeItemDyn :: forall t a r. Newtype t { activeItemDyn :: a | r } => Lens' t a
_activeItemDyn = _Newtype <<< prop (Proxy :: Proxy "activeItemDyn")


newtype BuilderUIEvents = BuilderUIEvents {
    buttons       :: ButtonsPane,
    slopeSelected :: Event SlopeOption,
    deleted       :: Event Unit,
    buildTree     :: Event Boolean,
    buildChimney  :: Event Boolean
    }

derive instance newtypeBuilderUIEvents :: Newtype BuilderUIEvents _
instance defaultBuilderUIEvents :: Default BuilderUIEvents where
    def = BuilderUIEvents {
        buttons       : def,
        slopeSelected : empty,
        deleted       : empty,
        buildTree     : empty,
        buildChimney  : empty
        }

savingStepDialog :: S.Dynamic SavingStep -> Widget Unit
savingStepDialog stepDyn = do
    div [attrsD $ dialogAttr <<< stepMode <$> stepDyn,
         classes ["uk-box-shadow-medium"]] $
        el "form" [] $
            div [class_ "uk-modal-body"] $ dynText $ show <$> stepDyn

-- | build the house editor UI widget system
houseBuilderUI :: BuilderUIConf -> Widget BuilderUIEvents
houseBuilderUI cfg = do
    let style s = mkStyle [ "position"       :~ "absolute",
                            "width"          :~ (show (s ^. _width) <> "px"),
                            "height"         :~ (show (s ^. _height) <> "px"),
                            "left"           :~ "0",
                            "top"            :~ "0",
                            "pointer-events" :~ "none" ]
    sizeD <- liftEffect $ toUIDyn $ cfg ^. _sizeDyn
    div [attrsD $ style <$> sizeD, class_ "uk-inline"] do
        showD <- liftEffect $ toUIDyn $ cfg ^. _showSaveDyn
        showR <- liftEffect $ toUIDyn $ cfg ^. _showResetDyn
        stepD <- liftEffect $ toUIDyn $ cfg ^. _savingStepDyn
        
        savingStepDialog stepD
        
        btns <- buttons $ def # _showSaveDyn  .~ showD
                              # _showCloseDyn .~ pure true
                              # _showResetDyn .~ showR
                              # _showUndoDyn  .~ showR

        editEvts <- editPane $ cfg ^. _activeItemDyn

        pure $ def # _buttons       .~ btns
                   # _slopeSelected .~ (editEvts ^. _activeItem <<< _slopeSelected)
                   # _deleted       .~ (editEvts ^. _activeItem <<< _deleted)
                   # _buildTree     .~ (editEvts ^. _buildTree)
                   # _buildChimney  .~ (editEvts ^. _buildChimney)
