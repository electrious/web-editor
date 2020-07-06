module Algorithm.SegmentRow where

import Prelude

import Algorithm.Segment (Segment, _startX, intersects)
import Data.Array (foldl, sortBy)
import Data.Default (class Default)
import Data.Foldable (class Foldable)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (round, toNumber)
import Data.Lens (Lens', (^.), (%~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (toUnfoldable)
import Data.Map (empty, insert, lookup, update, values)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, fromMaybe)
import Editor.Common.Lenses (_y)

newtype SegmentRow a = SegmentRow {
    segments :: Array (Segment a),
    y        :: Number
}

derive instance newtypeSegmentRow :: Newtype (SegmentRow a) _
instance defaultSegmentRow :: Default (SegmentRow a) where
    def = mkSegmentRow [] 0.0
instance functorSegmentRow :: Functor SegmentRow where
    map f sr = mkSegmentRow (map f <$> (sr ^. _segments)) (sr ^. _y)
_segments :: forall t a r. Newtype t { segments :: a | r } => Lens' t a
_segments = _Newtype <<< prop (SProxy :: SProxy "segments")

mkSegmentRow :: forall a. Array (Segment a) -> Number -> SegmentRow a
mkSegmentRow ss y = SegmentRow { segments: ss, y: y }

sortSegments :: forall a. SegmentRow a -> SegmentRow a
sortSegments sr = sr # _segments %~ sortBy f
    where f s1 s2 = compare (s1 ^. _startX) (s2 ^. _startX)

-- | merge all segments in the same row
mergeSegments :: forall a. Semigroup a => SegmentRow a -> SegmentRow a
mergeSegments sr = mkSegmentRow (segs <> fromMaybe lastSeg) (sr ^. _y)
    where ss = sortSegments sr
          f (Tuple segs Nothing)  s = Tuple segs (Just s)
          f (Tuple segs (Just l)) s = if intersects l s
                                      then Tuple segs (Just $ l <> s)
                                      else Tuple (segs <> [l]) (Just s)
          Tuple segs lastSeg = foldl f (Tuple [] Nothing) (ss ^. _segments)

-- | group an array of segments into different rows
groupSegments :: forall a f g. Foldable f => Unfoldable g => f (Segment a) -> g (SegmentRow a)
groupSegments segs = let f m s = let y = round ((s ^. _y) * 10000.0)
                                 in case lookup y m of
                                    Nothing -> insert y [s] m
                                    Just r  -> update (const $ Just $ r <> [s]) y m
                         dict = foldl f empty segs
                         mkRow y ss = mkSegmentRow ss (toNumber y)
                      in toUnfoldable $ values $ mapWithIndex mkRow dict
