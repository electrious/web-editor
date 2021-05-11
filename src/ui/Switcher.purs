module UI.Switcher where

import Prelude hiding (div)

import Data.Array (fromFoldable)
import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable, traverse)
import Editor.PanelNode (PanelOpacity(..))
import Model.ActiveMode (ActiveMode(..), isActive)
import Model.Roof.Panel (Alignment(..))
import Specular.Dom.Element (class_, text)
import Specular.Dom.Widget (Widget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (Dynamic, Event, leftmost, weaken)
import UI.Utils (div, mkAttrs, (:~))

class Eq o <= SwitcherOption o where
    optionLabel :: o -> String

instance switcherOptionBoolean :: SwitcherOption Boolean where
    optionLabel true  = "On"
    optionLabel false = "Off"

instance switcherOptionAlignment :: SwitcherOption Alignment where
    optionLabel Grid  = "Grid"
    optionLabel Brick = "Brick"

instance switcherOptionPanelOpacity :: SwitcherOption PanelOpacity where
    optionLabel Transparent = "20%"
    optionLabel Opaque      = "100%"


-- build a Switcher widget to switch between different options
switcher :: forall f o. Functor f => Foldable f => Traversable f => SwitcherOption o => f o -> Dynamic o -> Dynamic ActiveMode -> Widget (Event o)
switcher opts actOpDyn modeDyn =
    div [class_ "uk-button-group"] $ leftmost <<< fromFoldable <$> traverse (switcherBtn actOpDyn modeDyn) opts


switcherBtn :: forall o. SwitcherOption o => Dynamic o -> Dynamic ActiveMode -> o -> Widget (Event o)
switcherBtn actOpDyn modeDyn o = (map $ const o) <$> buttonOnClick attD (text $ optionLabel o)
    where attD = weaken $ mkAtt <$> actOpDyn <*> modeDyn
          mkAtt actO Active   = mkAttrs [ "class" :~ cls actO]
          mkAtt actO Inactive = mkAttrs [ "class" :~ cls actO,
                                          "disabled" :~ "" ]

          cls actO = if actO == o
                     then "uk-button uk-button-primary"
                     else "uk-button uk-button-default"
