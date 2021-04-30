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
import Specular.Dom.Browser ((:=))
import Specular.Dom.Element (attrsD)
import Specular.Dom.Widget (Widget, emptyWidget)
import UI.Bridge (toUIDyn)
import UI.Utils (div)

newtype BuilderUIConf = BuilderUIConf {
    sizeDyn       :: Dynamic Size,
    showExportDyn :: Dynamic Boolean
    }

derive instance newtypeBuilderUIConf :: Newtype BuilderUIConf _
instance defaultBuilderUIConf :: Default BuilderUIConf where
    def = BuilderUIConf {
        sizeDyn       : pure (size 10 10),
        showExportDyn : pure false
        }
_showExportDyn :: forall t a r. Newtype t { showExportDyn :: a | r } => Lens' t a
_showExportDyn = _Newtype <<< prop (SProxy :: SProxy "showExportDyn")

-- | build the house editor UI widget system
houseBuilderUI :: BuilderUIConf -> Widget Unit
houseBuilderUI cfg = do
    sizeD <- liftEffect $ toUIDyn $ cfg ^. _sizeDyn
    let style s = "style" := ("position: absolute; " <>
                              "width: " <> show (s ^. _width) <> "px;" <>
                              "height: " <> show (s ^. _height) <> "px;" <>
                              "padding: 8px; left: 0; top: 0; pointer-events: none;")
    div [attrsD $ style <$> sizeD] emptyWidget
