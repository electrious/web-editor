module WebPB.Map (MapPB, fromMapPB, toMapPB) where

import Prelude

import Data.Foldable (for_)
import Data.Map (Map, fromFoldable, toUnfoldable)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Util (ffi, fpi)

foreign import data MapPB :: Type -> Type -> Type
foreign import mkMapPB :: forall k v. Effect (MapPB k v)

toArray :: forall k v a. MapPB k v -> Array a
toArray = ffi ["m"] "m.toArray()"

set :: forall k v. k -> v -> MapPB k v -> Effect Unit
set = fpi ["k", "v", "m", ""] "m.set(k, v)"

key :: forall a k. a -> k
key = ffi ["a"] "a[0]"

value :: forall a v. a -> v
value = ffi ["a"] "a[1]"

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
