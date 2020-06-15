module Editor.PanelArrayLayout where

import Prelude

import Effect (Effect)
import Model.Roof.Panel (Panel, panelBBox)
import RBush.RBush (RBush, load, mkRBush)

mkRTree :: Array Panel -> Effect (RBush Panel)
mkRTree ps = do
    t <- mkRBush
    load (panelBBox <$> ps) t
    pure t