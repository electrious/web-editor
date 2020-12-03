module Rendering.NodeRenderable where

import Rendering.Node (Node)

class IsRenderable v rv where
    toRenderable   :: v -> rv
    fromRenderable :: rv -> v

class NodeRenderable e a b where
    render :: a -> Node e b
