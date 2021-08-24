module Data.UUIDMap where

import Prelude

import Data.Compactable (compact)
import Data.Foldable (class Foldable)
import Data.Int as Int
import Data.Lens ((^.))
import Data.List (List)
import Data.List as L
import Data.Map (Map, values)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as S
import Data.Tuple (Tuple(..))
import Data.UUIDWrapper (UUID)
import Data.UUIDWrapper as U
import Data.Unfoldable (class Unfoldable)
import Foreign.Object (Object)
import Foreign.Object as Obj
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


class IsStringKey a where
    toString :: a -> String
    fromString :: String -> Maybe a

instance IsStringKey UUID where
    toString = U.toString
    fromString = U.parseUUID

instance IsStringKey Int where
    toString = show
    fromString = Int.fromString


toObject :: forall k v. IsStringKey k => Map k v -> Object v
toObject = Obj.fromFoldable <<< map updK <<< f
    where updK (Tuple k v) = Tuple (toString k) v
          f :: Map k v -> List (Tuple k v)
          f = M.toUnfoldable


fromObject :: forall k v. Ord k => IsStringKey k => Object v -> Map k v
fromObject = M.fromFoldable <<< compact <<< map updK <<< f
    where updK (Tuple k v) = case fromString k of
                                 Just o -> Just $ Tuple o v
                                 Nothing -> Nothing
          f :: Object v -> List (Tuple String v)
          f = Obj.toUnfoldable
