module SmartHouse.UI where

import Prelude hiding (div)

import API.SmartHouse (SavingStep(..), stepMode)
import Data.Default (class Default, def)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_height, _width)
import Editor.Editor (_sizeDyn)
import Editor.SceneEvent (Size, size)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic)
import Specular.Dom.Element (attrsD, class_, classes, dynText, el)
import Specular.Dom.Widget (Widget)
import Specular.FRP as S
import UI.Bridge (toUIDyn)
import UI.ButtonPane (ButtonsPane, _showCloseDyn, _showResetDyn, _showSaveDyn, buttons)
import UI.ConfirmDialog (dialogAttr)
import UI.Utils (div, mkStyle, (:~))

newtype BuilderUIConf = BuilderUIConf {
    sizeDyn       :: Dynamic Size,
    showSaveDyn   :: Dynamic Boolean,
    showResetDyn  :: Dynamic Boolean,
    savingStepDyn :: Dynamic SavingStep
    }

derive instance newtypeBuilderUIConf :: Newtype BuilderUIConf _
instance defaultBuilderUIConf :: Default BuilderUIConf where
    def = BuilderUIConf {
        sizeDyn       : pure (size 10 10),
        showSaveDyn   : pure false,
        showResetDyn  : pure false,
        savingStepDyn : pure NotSaving
        }

_savingStepDyn :: forall t a r. Newtype t { savingStepDyn :: a | r } => Lens' t a
_savingStepDyn = _Newtype <<< prop (SProxy :: SProxy "savingStepDyn")


savingStepDialog :: S.Dynamic SavingStep -> Widget Unit
savingStepDialog stepDyn = do
    div [attrsD $ dialogAttr <<< stepMode <$> stepDyn,
         classes ["uk-box-shadow-medium"]] $
        el "form" [] $
            div [class_ "uk-modal-body"] $ dynText $ show <$> stepDyn

-- | build the house editor UI widget system
houseBuilderUI :: BuilderUIConf -> Widget ButtonsPane
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
        
        buttons $ def # _showSaveDyn  .~ showD
                      # _showCloseDyn .~ pure true
                      # _showResetDyn .~ showR
