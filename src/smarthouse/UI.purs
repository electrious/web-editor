module SmartHouse.UI where

import Prelude hiding (div)

import Control.Alternative (empty)
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
import FRP.Event (Event)
import Specular.Dom.Browser ((:=))
import Specular.Dom.Builder.Class (text)
import Specular.Dom.Element (attrsD, class_, classes)
import Specular.Dom.Widget (Widget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (weaken)
import Specular.FRP as S
import UI.Bridge (fromUIEvent, toUIDyn)
import UI.Utils (div, mkAttrs, mkStyle, (:~))

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

newtype BuilderUIEvents = BuilderUIEvents {
    toSave :: Event Unit
    }

derive instance newtypeBuilderUIEvents :: Newtype BuilderUIEvents _
instance defaultBuilderUIEvents :: Default BuilderUIEvents where
    def = BuilderUIEvents {
        toSave : empty
        }

_toSave :: forall t a r. Newtype t { toSave :: a | r } => Lens' t a
_toSave = _Newtype <<< prop (SProxy :: SProxy "toSave")

-- | build the house editor UI widget system
houseBuilderUI :: BuilderUIConf -> Widget BuilderUIEvents
houseBuilderUI cfg = do
    sizeD <- liftEffect $ toUIDyn $ cfg ^. _sizeDyn
    showD <- liftEffect $ toUIDyn $ cfg ^. _showSaveDyn
    let style s = mkStyle [ "position"       :~ "absolute",
                            "width"          :~ (show (s ^. _width) <> "px"),
                            "height"         :~ (show (s ^. _height) <> "px"),
                            "left"           :~ "0",
                            "top"            :~ "0",
                            "pointer-events" :~ "none" ]
    saveEvtUI <- div [attrsD $ style <$> sizeD, class_ "uk-inline"] $
                         div [classes ["uk-position-bottom-right",
                                       "uk-margin-small-right",
                                       "uk-margin-small-bottom"]] $ saveBtn showD
    saveEvt <- fromUIEvent saveEvtUI
    pure $ def # _toSave .~ saveEvt


-- the Save button
saveBtn :: S.Dynamic Boolean -> Widget (S.Event Unit)
saveBtn showDyn = buttonOnClick (weaken $ attD <$> showDyn) $ text "Save"
    where attD s = if s
                   then "class" := "uk-button"
                   else mkAttrs [ "class" :~ "uk-button",
                                  "disabled" :~ ""]
