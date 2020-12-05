module Rendering.DynamicNode where

import Prelude

import Control.Monad.Reader (ask)
import Control.Monad.Writer (tell)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Editor.Disposable (Disposee(..), dispose)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, performDynamic, subscribeDyn, withLast)
import FRP.Event (Event, subscribe)
import FRP.Event as Evt
import FRP.Event.Extra (performEvent)
import Rendering.Node (Node, runNode)
import Rendering.NodeRenderable (class NodeRenderable, render)

-- | run an event Node action in a Node context
eventNode :: forall e a. Event (Node e a) -> Node e (Event a)
eventNode evt = do
    env <- ask

    let rEvt = performEvent $ flip runNode env <$> evt
        resEvt = fst <$> rEvt

        getLast o = o.last

        f Nothing = pure unit
        f (Just d) = dispose d
    d <- liftEffect $ subscribe (Evt.withLast $ snd <$> rEvt) (getLast >>> f)
    tell $ Disposee d

    pure resEvt

-- | run an event node action in a node context and omit the result
eventNode_ :: forall e a. Event (Node e a) -> Node e Unit
eventNode_ = void <<< eventNode

-- | render an event stream of NodeRenderable value in Node
renderEvent :: forall e a b. NodeRenderable e a b => Event a -> Node e (Event b)
renderEvent = eventNode <<< map render

-- | run a dynamic Node action in a Node context
dynamic :: forall e a. Dynamic (Node e a) -> Node e (Dynamic a)
dynamic dyn = do
    env <- ask

    let rDyn       = performDynamic $ flip runNode env <$> dyn
        resDyn     = fst <$> rDyn

        getLast o  = o.last

        f Nothing  = pure unit
        f (Just d) = dispose d
        
    d <- liftEffect $ subscribeDyn (withLast $ snd <$> rDyn) (getLast >>> f)
    tell $ Disposee d
    
    pure resDyn

-- | run a dynamic node action in a node context and omit the result
dynamic_ :: forall e a. Dynamic (Node e a) -> Node e Unit
dynamic_ = void <<< dynamic

-- | render a dynamic value in Node
renderDynamic :: forall e a b. NodeRenderable e a b => Dynamic a -> Node e (Dynamic b)
renderDynamic = dynamic <<< map render
