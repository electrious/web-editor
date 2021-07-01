module UI.EditPane where

import Prelude hiding (div)

import Control.Plus (empty)
import Data.Default (class Default, def)
import Data.Lens (Lens', (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_roof)
import FRP.Dynamic (Dynamic)
import FRP.Event (Event)
import Model.SmartHouse.Roof (Roof)
import SmartHouse.ActiveRoofUI (ActiveRoofUI, activeRoofUI)
import Specular.Dom.Browser (Attrs)
import Specular.Dom.Element (attrs, classes, text)
import Specular.Dom.Widget (Widget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP as S
import UI.Bridge (fromUIEvent)
import UI.Utils (div, mkAttrs, mkStyle, (:~))

newtype EditPane = EditPane {
    buildTree :: Event Unit,
    roof      :: ActiveRoofUI
}

derive instance newtypeEditPane :: Newtype EditPane _
instance defaultEditPane :: Default EditPane where
    def = EditPane {
        buildTree : empty,
        roof      : def
    }

_buildTree :: forall t a r. Newtype t { buildTree :: a | r } => Lens' t a
_buildTree = _Newtype <<< prop (SProxy :: SProxy "buildTree")


-- | button to allow user to build a new tree
treeBtn :: Widget (S.Event Unit)
treeBtn = buttonOnClick (pure $ mkAttrs ["class" :~ "uk-button"]) (text "Build a Tree")

-- | a section in the UI to house editing buttons
editPaneStyle :: Boolean -> Attrs
editPaneStyle d = mkStyle [
    "position"       :~ "absolute",
    "background"     :~ "white",
    "width"          :~ "220px",
    "top"            :~ "80px",
    "right"          :~ "20px",
    "padding"        :~ "5px",
    "z-index"        :~ "10",
    "pointer-events" :~ "auto",
    "display"        :~ if d then "flex" else "none"
]

editPane :: Dynamic (Maybe Roof) -> Widget EditPane
editPane actRoofDyn =
    div [classes ["uk-flex", "uk-flex-column"], attrs (editPaneStyle true)] do
        treeEvt <- fromUIEvent =<< treeBtn

        roofEvt <- activeRoofUI actRoofDyn

        pure $ def # _buildTree .~ treeEvt
                   # _roof      .~ roofEvt
