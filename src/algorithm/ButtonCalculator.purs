module Algorithm.ButtonCalculator where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Compactable (compact)
import Data.Default (def)
import Data.Lens (view, (^.), (.~))
import Data.List (List(..), concat, filterM, fromFoldable, null, singleton, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Meter (meter, meterVal)
import Data.Traversable (traverse)
import Data.UUID (genUUID)
import Editor.Common.Lenses (_alignment, _arrayNumber, _config, _id, _orientation, _panels, _rows, _slope, _x, _y)
import Effect (Effect)
import Model.PanelArray (PanelArray)
import Model.PlusButton (PlusButton)
import Model.Roof.ArrayConfig (ArrayConfig, _panelSlope, colDistance, rowDistance)
import Model.Roof.Panel (Alignment(..), Panel, _arrNumber, _row_number, validatedSlope)
import Model.Roof.RoofPlate (RoofPlate)
import Model.RoofComponent (compBBox)
import RBush.RBush (RBush, insert, mkRBush, search)
import Three.Math.Vector (mkVec2, vecX, vecY)

plusBtnForEmptyArr :: PanelArray -> RoofPlate -> Effect PlusButton
plusBtnForEmptyArr arr roof = do
    id <- genUUID
    -- use default center.
    -- TODO: implement PolygonCenter algorithm and calculate the best
    -- position for the default plus button
    let center = mkVec2 0.0 0.0 
    pure $ def # _id          .~ id
               # _x           .~ meter (vecX center)
               # _y           .~ meter (vecY center)
               # _orientation .~ arr ^. _orientation
               # _slope       .~ arr ^. _config ^. _panelSlope
               # _arrayNumber .~ arr ^. _arrayNumber

sameRowPlusBtns :: ArrayConfig -> Panel -> Effect (List PlusButton)
sameRowPlusBtns cfg p = do
    i1 <- genUUID
    i2 <- genUUID
    let x = p ^. _x
        y = p ^. _y
        w = colDistance cfg p
        b = def # _y           .~ y
                # _orientation .~ p ^. _orientation
                # _slope       .~ p ^. _slope
                # _arrayNumber .~ p ^. _arrNumber
        b1 = b # _id .~ i1
               # _x  .~ x - w
        b2 = b # _id .~ i2
               # _x  .~ x + w
    pure (b1 : b2 : Nil)

otherPlusBtns :: ArrayConfig -> PanelArray -> RoofPlate -> Panel -> Effect (List PlusButton)
otherPlusBtns cfg arr roof p = do
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
    i1 <- genUUID
    i2 <- genUUID
    i3 <- genUUID
    i4 <- genUUID
    if arr ^. _alignment == Grid
      then do
        let rows = arr ^. _rows
            bpOrient = maybe o (view _orientation) (rows !! (rowNum - 1))
            tpOrient = maybe o (view _orientation) (rows !! (rowNum + 1))
            btn1 = if bpOrient == o
                   then Just $ b # _id .~ i1
                                 # _x  .~ x
                                 # _y  .~ y - h
                   else Nothing
            btn2 = if tpOrient == o
                   then Just $ b # _id .~ i2
                                 # _x  .~ x
                                 # _y  .~ y + h
                   else Nothing
        pure $ compact (btn1 : btn2 : Nil)
      else pure $ fromFoldable [
          b # _id .~ i1
            # _x  .~ x - w2
            # _y  .~ y - h
          ,
          b # _id .~ i2
            # _x  .~ x + w2
            # _y  .~ y - h
          ,
          b # _id .~ i3
            # _x  .~ x - w2
            # _y  .~ y + h
          ,
          b # _id .~ i4
            # _x  .~ x + w2
            # _y  .~ y + h
      ]


plusBtnsForArray :: PanelArray -> RBush Panel -> RoofPlate -> Effect (List PlusButton)
plusBtnsForArray arr pTree roof = do
    let ps = arr ^. _panels
        cfg = arr ^. _config
    if null ps
        then singleton <$> plusBtnForEmptyArr arr roof
        else do
            btns1 <- traverse (sameRowPlusBtns cfg) ps
            btns2 <- traverse (otherPlusBtns cfg arr roof) ps
            
            let btns = concat $ btns1 <> btns2
            btnTree <- mkRBush
            flip filterM btns (\btn -> do
                let pbBox  = compBBox btn (validatedSlope btn)
                    oldPBs = search pbBox btnTree
                    panels = search pbBox pTree
                
                if Array.null oldPBs && Array.null panels
                    then insert pbBox btnTree *> pure true
                    else pure false
            )
