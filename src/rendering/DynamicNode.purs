module Rendering.DynamicNode where

import Prelude

import Control.Monad.Reader (ask)
import Control.Monad.Writer (tell)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Editor.Disposable (Disposee(..), dispose)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, performDynamic, subscribeDyn, withLast)
import Rendering.Node (Node, runNode)
import Rendering.NodeRenderable (class NodeRenderable, render)

-- | run a dynamic Node action in a Node context
dynamic :: forall a. Dynamic (Node a) -> Node (Dynamic a)
dynamic dyn = do
    parent <- ask

    let rDyn       = performDynamic $ flip runNode parent <$> dyn
        resDyn     = fst <$> rDyn

        getLast o  = o.last

        f Nothing  = pure unit
        f (Just d) = dispose d
        
    d <- liftEffect $ subscribeDyn (withLast $ snd <$> rDyn) (getLast >>> f)
    tell $ Disposee d
    
    pure resDyn

-- | run a dynamic node action in a node context and omit the result
dynamic_ :: forall a. Dynamic (Node a) -> Node Unit
dynamic_ = void <<< dynamic

-- | render a dynamic value in Node
renderDynamic :: forall a b. NodeRenderable a b => Dynamic a -> Node (Dynamic b)
renderDynamic dyn = dynamic $ render <$> dyn
