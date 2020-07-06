module Algorithm.Segment where

import Prelude

import Data.Default (class Default, def)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_y)
import Math (abs)

newtype Segment a = Segment {
    startX  :: Number,
    endX    :: Number,
    y       :: Number,
    segData :: a
}
derive instance newtypeSegment :: Newtype (Segment a) _
instance functorSegment :: Functor Segment where
    map f s = mkSegment (s ^. _startX) (s ^. _endX) (s ^. _y) (f $ s ^. _segData)
instance semigroupSegment :: Semigroup a => Semigroup (Segment a) where
    append s1 s2 = Segment {
                       startX  : min (s1 ^. _startX) (s2 ^. _startX),
                       endX    : max (s1 ^. _endX) (s2 ^. _endX),
                       y       : s1 ^. _y,
                       segData : s1 ^. _segData <> s2 ^. _segData
                    }

instance defaultSegment :: Default a => Default (Segment a) where
    def = mkSegment 0.0 0.0 0.0 def

_startX :: forall t a r. Newtype t { startX :: a | r } => Lens' t a
_startX = _Newtype <<< prop (SProxy :: SProxy "startX")

_endX :: forall t a r. Newtype t { endX :: a | r } => Lens' t a
_endX = _Newtype <<< prop (SProxy :: SProxy "endX")

_segData :: forall t a r. Newtype t { segData :: a | r } => Lens' t a
_segData = _Newtype <<< prop (SProxy :: SProxy "segData")

mkSegment :: forall a. Number -> Number -> Number -> a -> Segment a
mkSegment sx ex y d = Segment {
    startX  : sx,
    endX    : ex,
    y       : y,
    segData : d
}

sameRow :: forall a. Segment a -> Segment a -> Boolean
sameRow s1 s2 = s1 ^. _y == s2 ^. _y

length :: forall a. Segment a -> Number
length s = s ^. _endX - s ^. _startX

intersects :: forall a. Segment a -> Segment a -> Boolean
intersects s1 s2 = sameRow s1 s2 && (equal sx1 ex2 || equal sx2 ex1 || not (ex1 < sx2 || sx1 > ex2))
    where sx1 = s1 ^. _startX
          ex1 = s1 ^. _endX
          sx2 = s2 ^. _startX
          ex2 = s2 ^. _endX

equal :: Number -> Number -> Boolean
equal n1 n2 = abs (n1 - n2) < 0.0001