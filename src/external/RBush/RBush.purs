module RBush.RBush (BBox(..), RBush, mkRBush, load, insert, search) where

import Prelude

import Data.Default (class Default, def)
import Data.Lens ((^.))
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_item)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)

newtype BBox a = BBox {
    minX :: Number,
    minY :: Number,
    maxX :: Number,
    maxY :: Number,
    item :: a
}
derive instance newtypeBBox :: Newtype (BBox a) _
instance defaultBBox :: Default a => Default (BBox a) where
    def = BBox {
        minX: 0.0,
        minY: 0.0,
        maxX: 0.0,
        maxY: 0.0,
        item: def
    }

foreign import data RBush :: Type -> Type

foreign import mkRBush :: forall a. Effect (RBush a)

instance defaultRBush :: Default (RBush a) where
    def = unsafePerformEffect mkRBush

foreign import load :: forall a. Array (BBox a) -> RBush a -> Effect Unit
foreign import insert :: forall a. BBox a -> RBush a -> Effect Unit
foreign import doSearch :: forall a b. BBox a -> RBush b -> Array (BBox b)

search :: forall a b. BBox a -> RBush b -> Array b
search b t = (_ ^. _item) <$> doSearch b t
