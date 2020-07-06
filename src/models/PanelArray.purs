module Model.PanelArray where

import Prelude
import Data.Foldable (class Foldable)
import Data.Meter (Meter)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)
import Model.ArrayRow (ArrayRow)
import Model.Roof.ArrayConfig (ArrayConfig)
import Model.Roof.Panel (Alignment, Orientation, Panel, panelSegment)
import Model.UpdatedPanels (UpdatedPanels, fromFoldable)

newtype PanelArray = PanelArray {
    arrayNumber   :: Int,
    panels        :: Array Panel,
    config        :: ArrayConfig,
    alignment     :: Alignment,
    orientation   :: Orientation,
    centerX       :: Meter,
    centerY       :: Meter,
    rows          :: Array ArrayRow,
    panelsUpdated :: UpdatedPanels
}
derive instance newtypePanelArray :: Newtype PanelArray _


splitToRows :: forall f g. Foldable f => Unfoldable g => ArrayConfig -> f Panel -> Tuple (g ArrayRow) UpdatedPanels
splitToRows cfg ps = Tuple rs (fromFoldable upd)
    where segs = panelSegment cfg <$> ps
          