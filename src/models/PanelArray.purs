module Model.PanelArray where

import Prelude

import Algorithm.Segment (_segData)
import Algorithm.SegmentRow (SegmentRow, _segments, groupSegments, mergeSegments)
import Data.Array as Array
import Data.Foldable (class Foldable, foldl)
import Data.Int (toNumber)
import Data.Lens (view, (%~), (.~), (^.))
import Data.List (List(..), concatMap, head, mapWithIndex, sortBy, toUnfoldable, (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromJust, maybe)
import Data.Meter (Meter, meter, meterVal)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unfoldable (class Unfoldable)
import Editor.Common.Lenses (_alignment, _arrayNumber, _centerX, _centerY, _config, _height, _orientation, _panels, _rackingType, _rowNumber, _rows, _width, _x, _y)
import Model.ArrayRow (ArrayRow, mkArrayRow)
import Model.PanelSegment (panelSegment)
import Model.Racking.RackingType (RackingType(..))
import Model.Roof.ArrayConfig (ArrayConfig, _gapX)
import Model.Roof.Panel (Alignment(..), Orientation(..), Panel, _arrNumber, _row_number, flipOrientation)
import Model.RoofComponent (size)
import Model.UpdatedPanels (UpdatedPanels, fromFoldable, merge)
import Partial.Unsafe (unsafePartial)

newtype PanelArray = PanelArray {
    arrayNumber   :: Int,
    panels        :: List Panel,
    config        :: ArrayConfig,
    alignment     :: Alignment,
    orientation   :: Orientation,
    centerX       :: Meter,
    centerY       :: Meter,
    rows          :: Array ArrayRow,
    panelsUpdated :: UpdatedPanels
}
derive instance newtypePanelArray :: Newtype PanelArray _

-- | make a simple panel array with provided orientation and array config only
simplePanelArray :: Orientation -> ArrayConfig -> PanelArray
simplePanelArray o cfg = PanelArray {
    arrayNumber   : 0,
    panels        : Nil,
    config        : cfg,
    alignment     : Grid,
    orientation   : o,
    centerX       : meter 0.0,
    centerY       : meter 0.0,
    rows          : [],
    panelsUpdated : fromFoldable []
}

mkPanelArray :: Int -> ArrayWithCenter -> ArrayConfig -> PanelArray
mkPanelArray arrNum arr cfg = PanelArray {
    arrayNumber   : arrNum,
    panels        : ps,
    config        : cfg,
    alignment     : guessAlignment ps,
    orientation   : if cfg ^. _rackingType == BX then Landscape else guessOrientation ps,
    centerX       : arr ^. _centerX,
    centerY       : arr ^. _centerY,
    rows          : rows,
    panelsUpdated : merge upd1 upd2
}
    where Tuple newPs upd1 = updateArrayNumbers (arr ^. _panels) arrNum
          Tuple rows upd2  = splitToRows cfg newPs
          ps = concatMap (view _panels) (List.fromFoldable rows)

mkPanelArrayMap :: forall f. Foldable f => Functor f => f PanelArray -> Map Int PanelArray
mkPanelArrayMap = Map.fromFoldable <<< map mkT
    where mkT a = Tuple (a ^. _arrayNumber) a

data Triple a b c = Triple a b c

tripleToTuple :: forall a b c. Triple a b c -> Tuple a b
tripleToTuple (Triple a b _) = Tuple a b

rotateRow :: PanelArray -> Int -> Tuple (List Panel) (List Panel)
rotateRow arr rowNum = tripleToTuple $ foldl f (Triple Nil Nil (meter 0.0)) (Array.sortBy (comparing (view _rowNumber)) $ arr ^. _rows)
    where f (Triple newPs toUpd c) r | r ^. _rowNumber == rowNum = rotate r newPs toUpd c
                                     | r ^. _rowNumber >  rowNum = updateY r newPs toUpd c
                                     | otherwise                 = Triple (newPs <> r ^. _panels) toUpd c

          rotate r newPs toUpd _ = let ps     = sortBy (comparing (view _x)) $ r ^. _panels
                                       p      = unsafePartial $ fromJust $ head ps
                                       sz     = size p
                                       w      = sz ^. _width
                                       h      = sz ^. _height
                                       change = w - h
                                       w2     = meter $ meterVal w / 2.0
                                       h2     = meter $ meterVal h / 2.0
                                       baseX  = p ^. _x - w2
                                       baseY  = p ^. _y - h2
                                       newY   = baseY + w2
                                       g (Triple nps ups c) (Tuple idx panel) = let newP = panel # _x .~ baseX + h2 + meter (meterVal (arr ^. _config ^. _gapX + h) * toNumber idx)
                                                                                                 # _y .~ newY
                                                                                                 # _orientation %~ flipOrientation
                                                                            in Triple (newP:nps) (newP:ups) c
                                   in foldl g (Triple newPs toUpd change) $ mapWithIndex Tuple $ r ^. _panels
          updateY r newPs toUpd c = let g (Triple nps ups change) p = let newP = p # _y %~ ((+) change)
                                                                      in Triple (newP:nps) (newP:ups) change
                                    in foldl g (Triple newPs toUpd c) $ r ^. _panels

newtype ArrayWithCenter = ArrayWithCenter {
    centerX :: Meter,
    centerY :: Meter,
    panels  :: List Panel
}

derive instance newtypeArrayWithCenter :: Newtype ArrayWithCenter _

mkArrayWithCenter :: forall f. Foldable f => f Panel -> ArrayWithCenter
mkArrayWithCenter ps = toArr $ foldl f (Triple 0.0 0.0 0) ps
    where f (Triple x y n) p = let nx = x + meterVal (p ^. _x)
                                   ny = y + meterVal (p ^. _y)
                               in Triple nx ny (n + 1)
          toArr (Triple x y n) = ArrayWithCenter {
              centerX : meter (x / toNumber n),
              centerY : meter (y / toNumber n),
              panels  : List.fromFoldable ps
          }

guessAlignment :: forall f. Foldable f => f Panel -> Alignment
guessAlignment ps = let f (Tuple g b) p = if p ^. _alignment == Grid
                                          then Tuple (g + 1) b
                                          else Tuple g (b + 1)
                        Tuple g b = foldl f (Tuple 0 0) ps
                    in if g >= b then Grid else Brick

guessOrientation :: forall f. Foldable f => f Panel -> Orientation
guessOrientation ps = let f (Tuple l p) panel = if panel ^. _orientation == Landscape
                                                then Tuple (l + 1) p
                                                else Tuple l (p + 1)
                          Tuple l p = foldl f (Tuple 0 0) ps
                      in if l >= p then Landscape else Portrait

updateArrayNumbers :: forall f. Foldable f => Unfoldable f => f Panel -> Int -> Tuple (f Panel) UpdatedPanels
updateArrayNumbers ps arrNum = let f (Tuple nps upd) p = if p ^. _arrNumber == arrNum
                                                         then Tuple (p:nps) upd
                                                         else let np = p # _arrNumber .~ arrNum
                                                              in Tuple (np:nps) (np:upd)
                                   Tuple newPs upd = foldl f (Tuple Nil Nil) ps
                               in Tuple (toUnfoldable newPs) (fromFoldable upd)

segRowToArraryRow :: Int -> SegmentRow (List Panel) -> Tuple ArrayRow (List Panel)
segRowToArraryRow idx row = let ps = concatMap (view _segData) (row ^. _segments)
                                f (Tuple nps upd) p = if p ^. _row_number == idx
                                                      then Tuple (p:nps) upd
                                                      else let np = p # _row_number .~ idx
                                                           in Tuple (np:nps) (np:upd)
                                Tuple newPs upd = foldl f (Tuple Nil Nil) ps
                                orient = maybe Landscape (view _orientation) (head newPs)
                            in Tuple (mkArrayRow idx orient newPs) upd

splitToRows :: forall f g. Foldable f => Functor f => Unfoldable g => ArrayConfig -> f Panel -> Tuple (g ArrayRow) UpdatedPanels
splitToRows cfg ps = Tuple (toUnfoldable newRows) (fromFoldable upds)
    where segs    = panelSegment cfg <$> ps
          rows    = mergeSegments    <$> groupSegments segs
          rs      = mapWithIndex segRowToArraryRow $ sortBy (comparing (view _y)) rows
          newRows = fst <$> rs
          upds    = concatMap snd rs
