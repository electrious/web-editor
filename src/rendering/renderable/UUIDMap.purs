module Rendering.Renderable.UUIDMap where

import Control.Alt (class Functor)
import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable, traverse)
import Data.UUIDMap (UUIDMap)
import Rendering.NodeRenderable (class NodeRenderable, render)



newtype UUIDMapRenderable a = UUIDMapRenderable (UUIDMap a)

derive newtype instance functorUUIDMapRenderable     :: Functor UUIDMapRenderable
derive newtype instance foldableUUIDMapRenderable    :: Foldable UUIDMapRenderable
derive newtype instance traversableUUIDMapRenderable :: Traversable UUIDMapRenderable

toRenderable :: forall a. UUIDMap a -> UUIDMapRenderable a
toRenderable = UUIDMapRenderable

fromRenderable :: forall a. UUIDMapRenderable a -> UUIDMap a
fromRenderable (UUIDMapRenderable m) = m

instance nodeRenderableUUIDMapRenderable :: NodeRenderable e a b => NodeRenderable e (UUIDMapRenderable a) (UUIDMapRenderable b) where
    render = traverse render
