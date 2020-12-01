module Rendering.NodeRenderable where

import Rendering.Node (Node)

class NodeRenderable a b where
    render :: a -> Node b
