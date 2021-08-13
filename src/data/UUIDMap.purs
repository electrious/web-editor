module Data.UUIDWrapperMap where

import Prelude

import Data.Foldable (class Foldable)
import Data.Lens ((^.))
import Data.List as L
import Data.Map (Map, values)
import Data.Map as M
import Data.Set (Set)
import Data.Set as S
import Data.Tuple (Tuple(..))
import Data.UUIDWrapper (UUID)
import Data.Unfoldable (class Unfoldable)
import Model.UUID (class HasUUID, idLens)

-- | UUIDMap is a map from UUID to any value that's instance of HasUUID
type UUIDMap a = Map UUID a

toIdTuple :: forall a. HasUUID a => a -> Tuple UUID a
toIdTuple a = Tuple (a ^. idLens) a

fromFoldable :: forall f a. Functor f => Foldable f => HasUUID a => f a -> UUIDMap a
fromFoldable = M.fromFoldable <<< map toIdTuple

fromSet :: forall a. Ord a => HasUUID a => Set a -> UUIDMap a
fromSet = M.fromFoldable <<< S.map toIdTuple

toUnfoldable :: forall f a. Unfoldable f => UUIDMap a -> f a
toUnfoldable = L.toUnfoldable <<< values
