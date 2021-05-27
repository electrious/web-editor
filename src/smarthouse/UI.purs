module SmartHouse.UI where

import Prelude hiding (div)

import Data.Default (class Default, def)
import Data.Lens ((.~), (^.))
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_height, _width)
import Editor.Editor (_sizeDyn)
import Editor.SceneEvent (Size, size)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic)
import Specular.Dom.Element (attrsD, class_)
import Specular.Dom.Widget (Widget)
import UI.Bridge (toUIDyn)
import UI.ButtonPane (ButtonsPane, _showCloseDyn, _showResetDyn, _showSaveDyn, buttons)
import UI.Utils (div, mkStyle, (:~))

newtype BuilderUIConf = BuilderUIConf {
    sizeDyn      :: Dynamic Size,
    showSaveDyn  :: Dynamic Boolean,
    showResetDyn :: Dynamic Boolean
    }

derive instance newtypeBuilderUIConf :: Newtype BuilderUIConf _
instance defaultBuilderUIConf :: Default BuilderUIConf where
    def = BuilderUIConf {
        sizeDyn      : pure (size 10 10),
        showSaveDyn  : pure false,
        showResetDyn : pure false
        }

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
        buttons $ def # _showSaveDyn  .~ showD
                      # _showCloseDyn .~ pure true
                      # _showResetDyn .~ showR
