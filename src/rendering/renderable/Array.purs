module Rendering.Renderable.Array where

import Control.Alt (class Functor)
import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable, traverse)
import Rendering.NodeRenderable (class NodeRenderable, render)


newtype ArrayRenderable a = ArrayRenderable (Array a)

derive newtype instance functorArrayRenderable     :: Functor ArrayRenderable
derive newtype instance foldableArrayRenderable    :: Foldable ArrayRenderable
derive newtype instance traversableArrayRenderable :: Traversable ArrayRenderable

toRenderable :: forall a. Array a -> ArrayRenderable a
toRenderable = ArrayRenderable

fromRenderable :: forall a. ArrayRenderable a -> Array a
fromRenderable (ArrayRenderable a) = a

instance nodeRenderableArrayRenderable :: NodeRenderable e a b => NodeRenderable e (ArrayRenderable a) (ArrayRenderable b) where
    render = traverse render
