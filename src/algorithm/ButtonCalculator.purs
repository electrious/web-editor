module Algorithm.ButtonCalculator where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Compactable (compact)
import Data.Default (def)
import Data.Foldable (class Foldable, foldl)
import Data.Lens (view, (^.), (.~))
import Data.List (List(..), concat, filterM, fromFoldable, head, null, singleton, (:))
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Meter (Meter, meter, meterVal)
import Editor.Common.Lenses (_alignment, _arrayNumber, _config, _orientation, _panels, _rowNumber, _rows, _slope, _width, _x, _y)
import Effect (Effect)
import Math (abs)
import Model.PanelArray (PanelArray)
import Model.PlusButton (PlusButton)
import Model.Roof.ArrayConfig (ArrayConfig, _panelSlope, colDistance, rowDistance)
import Model.Roof.Panel (Alignment(..), Panel, _arrNumber, _row_number, validatedSlope)
import Model.Roof.RoofPlate (RoofPlate)
import Model.RoofComponent (compBBox, compX, compY, size)
import Model.RotateButton (RotateButton)
import Model.UUID (assignNewIds)
import Partial.Unsafe (unsafePartial)
import RBush.RBush (RBush, insert, mkRBush, search)
import Three.Math.Vector (mkVec2, vecX, vecY)

plusBtnForEmptyArr :: PanelArray -> RoofPlate -> PlusButton
plusBtnForEmptyArr arr roof = def # _x           .~ meter (vecX center)
                                  # _y           .~ meter (vecY center)
                                  # _orientation .~ arr ^. _orientation
                                  # _slope       .~ arr ^. _config ^. _panelSlope
                                  # _arrayNumber .~ arr ^. _arrayNumber
    -- use default center.
    -- TODO: implement PolygonCenter algorithm and calculate the best
    -- position for the default plus button
    where center = mkVec2 0.0 0.0 


sameRowPlusBtns :: ArrayConfig -> Panel -> List PlusButton
sameRowPlusBtns cfg p = b1 : b2 : Nil
    where x = p ^. _x
          y = p ^. _y
          w = colDistance cfg p
          b = def # _y           .~ y
                  # _orientation .~ p ^. _orientation
                  # _slope       .~ p ^. _slope
                  # _arrayNumber .~ p ^. _arrNumber
          b1 = b # _x  .~ x - w
          b2 = b # _x  .~ x + w

otherPlusBtns :: ArrayConfig -> PanelArray -> RoofPlate -> Panel -> List PlusButton
otherPlusBtns cfg arr roof p =
    let x      = p ^. _x
        y      = p ^. _y
        w      = colDistance cfg p
        w2     = meter $ meterVal w / 2.0
        h      = rowDistance cfg p

        arrNum = p ^. _arrNumber
        rowNum = p ^. _row_number
        o      = p ^. _orientation
        -- default button
        b      = def # _orientation .~ o
                     # _slope       .~ p ^. _slope
                     # _arrayNumber .~ arrNum

    in if arr ^. _alignment == Grid
      then
        let rows = arr ^. _rows
            bpOrient = maybe o (view _orientation) (rows !! (rowNum - 1))
            tpOrient = maybe o (view _orientation) (rows !! (rowNum + 1))
            btn1 = if bpOrient == o
                   then Just $ b # _x  .~ x
                                 # _y  .~ y - h
                   else Nothing
            btn2 = if tpOrient == o
                   then Just $ b # _x  .~ x
                                 # _y  .~ y + h
                   else Nothing
        in compact (btn1 : btn2 : Nil)
      else fromFoldable [
          b # _x  .~ x - w2
            # _y  .~ y - h
          ,
          b # _x  .~ x + w2
            # _y  .~ y - h
          ,
          b # _x  .~ x - w2
            # _y  .~ y + h
          ,
          b # _x  .~ x + w2
            # _y  .~ y + h
      ]


plusBtnsForArray :: PanelArray -> RBush Panel -> RoofPlate -> Effect (List PlusButton)
plusBtnsForArray arr pTree roof = do
    let ps = arr ^. _panels
        cfg = arr ^. _config
    if null ps
        then assignNewIds $ singleton $ plusBtnForEmptyArr arr roof
        else do
            let btns1 = sameRowPlusBtns cfg <$> ps
                btns2 = otherPlusBtns cfg arr roof <$> ps
                btns = concat $ btns1 <> btns2
            btnTree <- mkRBush
            resBtns <- flip filterM btns (\btn -> do
                let pbBox  = compBBox btn (validatedSlope btn)
                    oldPBs = search pbBox btnTree
                    panels = search pbBox pTree
                
                -- TODO: validate the button is inside the roof
                if Array.null oldPBs && Array.null panels
                    then insert pbBox btnTree *> pure true
                    else pure false
            )
            assignNewIds resBtns


rotateBtnsForArray :: PanelArray -> List PlusButton -> Effect (List RotateButton)
rotateBtnsForArray arr pbs = assignNewIds $ if null ps then Nil else fromFoldable (f <$> rows)
    where ps = arr ^. _panels
          rows = arr ^. _rows
          f row = let rowPanels = row ^. _panels
                      p = unsafePartial $ fromJust $ head rowPanels
                      y = compY p
                      w = (size p) ^. _width
                      pos1 = rightEndOfPanels rowPanels
                      pos2 = rightEndOfBtns y pbs
                      pos = max pos1 pos2
                  in def # _x           .~ pos + w
                         # _y           .~ y
                         # _arrayNumber .~ arr ^. _arrayNumber
                         # _rowNumber   .~ row ^. _rowNumber
                         # _slope       .~ p ^. _slope


-- find the right end position of a list of panels
rightEndOfPanels :: forall f. Foldable f => f Panel -> Meter
rightEndOfPanels = foldl (\curX p -> if curX < compX p then compX p else curX) (meter $ -1000000000.0)

rightEndOfBtns :: forall f. Foldable f => Meter -> f PlusButton -> Meter
rightEndOfBtns y = foldl f (meter $ -1000000000.0)
    where f curX pb = let px = compX pb
                      in if abs (meterVal $ compY pb - y) < 0.01 && curX < px
                         then px
                         else curX
