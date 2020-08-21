module Model.PanelSegment where

import Prelude

import Algorithm.Segment (Segment, mkSegment)
import Data.Lens ((^.))
import Data.List (List, singleton)
import Data.Meter (meterVal)
import Editor.Common.Lenses (_width, _x, _y)
import Model.Roof.ArrayConfig (ArrayConfig, _gapX)
import Model.Roof.Panel (Panel)
import Model.RoofComponent (size)

panelSegment :: ArrayConfig -> Panel -> Segment (List Panel)
panelSegment cfg p = mkSegment (x - w2 - gapX2) (x + w2 + gapX2) y (singleton p)
    where s     = size p
          w2    = meterVal (s ^. _width) / 2.0
          gapX2 = meterVal (cfg ^. _gapX) / 2.0
          x     = meterVal $ p ^. _x
          y     = meterVal $ p ^. _y
