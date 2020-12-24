module Data.UUIDMap where

import Prelude

import Data.Foldable (class Foldable)
import Data.Lens ((^.))
import Data.List as L
import Data.Map (Map, values)
import Data.Map as M
import Data.Tuple (Tuple(..))
import Data.UUID (UUID)
import Data.Unfoldable (class Unfoldable)
import Model.UUID (class HasUUID, idLens)

-- | UUIDMap is a map from UUID to any value that's instance of HasUUID
type UUIDMap a = Map UUID a

fromFoldable :: forall f a. Functor f => Foldable f => HasUUID a => f a -> UUIDMap a
fromFoldable = M.fromFoldable <<< map f
    where f a = Tuple (a ^. idLens) a

toUnfoldable :: forall f a. Unfoldable f => UUIDMap a -> f a
toUnfoldable = L.toUnfoldable <<< values
