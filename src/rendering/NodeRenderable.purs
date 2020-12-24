module Rendering.NodeRenderable where


import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Rendering.Node (Node)

class NodeRenderable e a b where
    render :: a -> Node e b


instance nodeRenderableArray :: NodeRenderable e a b => NodeRenderable e (Array a) (Array b) where
    render = traverse render
instance nodeRenderableList :: NodeRenderable e a b => NodeRenderable e (List a) (List b) where
    render = traverse render
instance nodeRenderableMap :: NodeRenderable e a b => NodeRenderable e (Map k a) (Map k b) where
    render = traverse render
instance nodeRenderableMaybe :: NodeRenderable e a b => NodeRenderable e (Maybe a) (Maybe b) where
    render = traverse render
