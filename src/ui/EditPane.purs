module UI.EditPane where

import Prelude hiding (div)

import Control.Plus (empty)
import Data.Default (class Default, def)
import Data.Lens (Lens', (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Editor.Common.Lenses (_buildChimney, _buildTree)
import FRP.Dynamic (Dynamic)
import FRP.Event (Event)
import Models.SmartHouse.ActiveItem (ActiveItem)
import SmartHouse.ActiveItemUI (ActiveItemUI, activeItemUI)
import Specular.Dom.Browser (Attrs)
import Specular.Dom.Builder.Class (dynText)
import Specular.Dom.Element (attrs, classes)
import Specular.Dom.Widget (Widget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (attachDynWith, fixEvent, holdDyn, weaken)
import Specular.FRP as S
import Type.Proxy (Proxy(..))
import UI.Bridge (fromUIEvent)
import UI.Utils (div, mkAttrs, mkStyle, (:~))

newtype EditPane = EditPane {
    buildTree    :: Event Boolean,
    buildChimney :: Event Boolean,
    activeItem   :: ActiveItemUI
}

derive instance newtypeEditPane :: Newtype EditPane _
instance defaultEditPane :: Default EditPane where
    def = EditPane {
        buildTree    : empty,
        buildChimney : empty,
        activeItem   : def
    }

_activeItem :: forall t a r. Newtype t { activeItem :: a | r } => Lens' t a
_activeItem = _Newtype <<< prop (Proxy :: Proxy "activeItem")

-- | button to allow user to build a new tree
treeBtn :: Widget (S.Event Boolean)
treeBtn = toggleBtn label Nothing
    where label true  = "Stop Building Tree"
          label false = "Build a Tree"

-- | button to allow user to build a new chimney
chimneyBtn :: Widget (S.Event Boolean)
chimneyBtn = toggleBtn label (Just "uk-margin-top")
    where label true  = "Stop Building Chimney"
          label false = "Build a Chimney"

-- | button to allow user to toggle a state
toggleBtn :: (Boolean -> String) -> Maybe String -> Widget (S.Event Boolean)
toggleBtn label cls = fixEvent \tapEvt -> do
    d <- holdDyn false tapEvt
    e <- buttonOnClick (pure $ mkAttrs ["class" :~ ("uk-button " <> fromMaybe "" cls)]) (dynText $ weaken $ label <$> d)
    let nextEvt = attachDynWith (const <<< not) d e
    pure $ Tuple nextEvt nextEvt

-- | a section in the UI to house editing buttons
editPaneStyle :: Boolean -> Attrs
editPaneStyle d = mkStyle [
    "position"       :~ "absolute",
    "background"     :~ "white",
    "width"          :~ "250px",
    "top"            :~ "80px",
    "right"          :~ "20px",
    "padding"        :~ "5px",
    "z-index"        :~ "10",
    "pointer-events" :~ "auto",
    "display"        :~ if d then "flex" else "none"
]

editPane :: Dynamic (Maybe ActiveItem) -> Widget EditPane
editPane actItemDyn =
    div [classes ["uk-flex", "uk-flex-column"], attrs (editPaneStyle true)] do
        treeEvt <- fromUIEvent =<< treeBtn
        chimEvt <- fromUIEvent =<< chimneyBtn
        roofEvt <- activeItemUI actItemDyn

        pure $ def # _buildTree    .~ treeEvt
                   # _buildChimney .~ chimEvt
                   # _activeItem   .~ roofEvt
