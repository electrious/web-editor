module RBush.RBush where

import Prelude

import Effect (Effect)
import Util (ffi, fpi)

type BBox a = {
    minX :: Number,
    minY :: Number,
    maxX :: Number,
    maxY :: Number | a
}

foreign import data RBush :: Type -> Type

mkRBush :: forall a. Effect (RBush a)
mkRBush = ffi [""] "new RBush.RBush()"

load :: forall a. Array (BBox a) -> RBush (BBox a) -> Effect Unit
load = fpi ["items", "tree", ""] "tree.load(items)"

search :: forall a b. BBox a -> RBush (BBox b) -> Effect (Array (BBox b))
search = ffi ["box", "tree", ""] "tree.search(box)"
