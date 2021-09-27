module Rendering.Racking.GAFRendering where

import Prelude

import Data.Default (def)
import Data.Lens ((.~))
import Editor.Common.Lenses (_name)
import Model.Racking.GAF.GAFRackingComponent (GAFRackingComponent)
import Model.Racking.GAF.Hood (Hood)
import Taihe.Node (Node, node)

renderGAF :: forall e. GAFRackingComponent -> Node e Unit
renderGAF _ = node (def # _name .~ "GAFRackingComponent") $ pure unit

renderHood :: forall e. Hood -> Node e Unit
renderHood _ = pure unit