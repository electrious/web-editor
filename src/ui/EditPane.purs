module UI.EditPane where

import Prelude hiding (div)

import Control.Plus (empty)
import Data.Default (class Default, def)
import Data.Lens (Lens', (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_buildTree)
import FRP.Dynamic (Dynamic)
import FRP.Event (Event)
import Models.SmartHouse.ActiveItem (ActiveItem)
import SmartHouse.ActiveItemUI (ActiveItemUI, activeItemUI, toggleBtn)
import Specular.Dom.Browser (Attrs)
import Specular.Dom.Element (attrs, classes)
import Specular.Dom.Widget (Widget)
import Specular.FRP as S
import Type.Proxy (Proxy(..))
import UI.Bridge (fromUIEvent)
import UI.Utils (div, mkStyle, (:~))

newtype EditPane = EditPane {
    buildTree  :: Event Boolean,
    activeItem :: ActiveItemUI
}

derive instance newtypeEditPane :: Newtype EditPane _
instance defaultEditPane :: Default EditPane where
    def = EditPane {
        buildTree  : empty,
        activeItem : def
    }

_activeItem :: forall t a r. Newtype t { activeItem :: a | r } => Lens' t a
_activeItem = _Newtype <<< prop (Proxy :: Proxy "activeItem")

-- | button to allow user to build a new tree
treeBtn :: Widget (S.Event Boolean)
treeBtn = toggleBtn label Nothing
    where label true  = "Stop Building Tree"
          label false = "Build a Tree"

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
        roofEvt <- activeItemUI actItemDyn

        pure $ def # _buildTree    .~ treeEvt
                   # _activeItem   .~ roofEvt
