module Rendering.Renderable.List where

import Control.Alt (class Functor)
import Data.Foldable (class Foldable)
import Data.List (List)
import Data.Traversable (class Traversable, traverse)
import Rendering.NodeRenderable (class NodeRenderable, render)


newtype ListRenderable a = ListRenderable (List a)

derive newtype instance functorListRenderable     :: Functor ListRenderable
derive newtype instance foldableListRenderable    :: Foldable ListRenderable
derive newtype instance traversableListRenderable :: Traversable ListRenderable

toRenderable :: forall a. List a -> ListRenderable a
toRenderable = ListRenderable

fromRenderable :: forall a. ListRenderable a -> List a
fromRenderable (ListRenderable l) = l

instance nodeRenderableListRenderable :: NodeRenderable e a b => NodeRenderable e (ListRenderable a) (ListRenderable b) where
    render = traverse render
