module Algorithm.PanelAligning (alignPanelRows) where

import Prelude

import Data.Foldable (foldl)
import Data.Int (odd)
import Data.Lens ((^.), (%~), (.~))
import Data.List (List(..), sort)
import Data.List.Partial (head)
import Data.Map.Internal (keys, lookup)
import Data.Maybe (Maybe(..))
import Data.Meter (Meter, meter, meterVal)
import Editor.Common.Lenses (_alignment, _panels, _width, _x)
import Model.ArrayRow (ArrayRow, rowsToMap)
import Model.Roof.Panel (Alignment(..), Panel)
import Model.RoofComponent (size)
import Partial.Unsafe (unsafePartial)

-- | get offset for alignment
alignOffset :: Panel -> Alignment -> Meter
alignOffset p algn = case algn of
                        Grid  -> meter $ -offset
                        Brick -> meter offset
    where offset = meterVal ((size p) ^. _width) / 2.0

alignPanelRows :: Alignment -> List ArrayRow -> List Panel
alignPanelRows _    Nil  = Nil
alignPanelRows algn rows = foldl f Nil rowNums
    where row     = unsafePartial $ head rows
          panel   = unsafePartial $ head $ row ^. _panels
          offset  = alignOffset panel algn
          rowDict = rowsToMap rows
          rowNums = sort $ keys rowDict
          updateOffset p = updateAlign $ p # _x %~ (+) offset
          updateAlign p  = p # _alignment .~ algn

          f upd rowNum = case lookup rowNum rowDict of
            Just r  -> if odd rowNum
                        then append upd $ updateOffset <$> r ^. _panels
                        else append upd $ updateAlign <$> r ^. _panels
            Nothing -> upd