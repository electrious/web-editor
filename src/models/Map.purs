module Model.MapPB (MapPB, fromMapPB, toMapPB) where

import Prelude

import Data.Foldable (for_)
import Data.Map (Map, fromFoldable, toUnfoldable)
import Data.Tuple (Tuple(..))
import Effect (Effect)

foreign import data MapPB :: Type -> Type -> Type
foreign import mkMapPB :: forall k v. Effect (MapPB k v)

foreign import toArray :: forall k v a. MapPB k v -> Array a
foreign import set :: forall k v. k -> v -> MapPB k v -> Effect Unit
foreign import key :: forall a k. a -> k
foreign import value :: forall a v. a -> v

fromMapPB :: forall k v. Ord k => MapPB k v -> Map k v
fromMapPB = fromFoldable <<< map toTuple <<< toArray
    where toTuple arr = Tuple (key arr) (value arr)

toMapPB :: forall k v. Ord k => Map k v -> Effect (MapPB k v)
toMapPB m = do
    resM <- mkMapPB
    let entries :: Array (Tuple k v)
        entries = toUnfoldable m
    
    for_ entries \(Tuple k v) -> set k v resM
    pure resM
