module Rendering.NodeRenderable where

import Rendering.Node (Node)

class NodeRenderable e a b where
    render :: a -> Node e b
