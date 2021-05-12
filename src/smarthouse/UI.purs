module SmartHouse.UI where

import Prelude hiding (div)

import Data.Default (class Default)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_height, _width)
import Editor.Editor (_sizeDyn)
import Editor.SceneEvent (Size, size)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic)
import FRP.Event (Event)
import Specular.Dom.Element (attrsD, class_)
import Specular.Dom.Widget (Widget)
import UI.Bridge (fromUIEvent, toUIDyn)
import UI.ButtonPane (ButtonClicked, buttons)
import UI.Utils (div, mkStyle, (:~))

newtype BuilderUIConf = BuilderUIConf {
    sizeDyn     :: Dynamic Size,
    showSaveDyn :: Dynamic Boolean
    }

derive instance newtypeBuilderUIConf :: Newtype BuilderUIConf _
instance defaultBuilderUIConf :: Default BuilderUIConf where
    def = BuilderUIConf {
        sizeDyn     : pure (size 10 10),
        showSaveDyn : pure false
        }

_showSaveDyn :: forall t a r. Newtype t { showSaveDyn :: a | r } => Lens' t a
_showSaveDyn = _Newtype <<< prop (SProxy :: SProxy "showSaveDyn")


-- | build the house editor UI widget system
houseBuilderUI :: BuilderUIConf -> Widget (Event ButtonClicked)
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
        fromUIEvent =<< buttons showD (pure true)
