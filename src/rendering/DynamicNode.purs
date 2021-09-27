module Rendering.DynamicNode (eventNode, eventNode_, renderEvent,
                              dynamic, dynamic_, renderDynamic) where

import Prelude

import Control.Monad.Reader (ask)
import Control.Monad.Writer (tell)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Taihe.Disposable (Disposee(..), dispose)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, current, performDynamic, step, subscribeDyn, withLast)
import FRP.Event (Event, subscribe)
import FRP.Event as Evt
import FRP.Event.Extra (multicast, performEvent)
import Rendering.Node (Node, runNode)
import Rendering.NodeRenderable (class NodeRenderable, render)


-- internal help functions
getLast :: forall a r. { last :: a | r } -> a
getLast o = o.last

-- | run an event Node action in a Node context
eventNode :: forall e a. Event (Node e a) -> Node e (Event a)
eventNode evt = do
    env <- ask

    let rEvt = performEvent $ flip runNode env <$> evt
        
        dispEvt = multicast $ snd <$> rEvt

        -- store the Disposee object for the current node action
        dispDyn = step Nothing $ Just <$> dispEvt
        -- function to dispose the current node action
        toDisposeCurVal = current dispDyn >>= traverse_ dispose
        
    d <- liftEffect $ subscribe (Evt.withLast dispEvt) (getLast >>> traverse_ dispose)
    
    tell $ Disposee $ d *> toDisposeCurVal

    pure $ fst <$> rEvt

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

    let rDyn    = performDynamic $ flip runNode env <$> dyn
        dispDyn = snd <$> rDyn

        toDisposeCurVal = current dispDyn >>= dispose

    d <- liftEffect $ subscribeDyn (withLast dispDyn) (getLast >>> traverse_ dispose)
    tell $ Disposee $ d *> toDisposeCurVal
    
    pure $ fst <$> rDyn

-- | run a dynamic node action in a node context and omit the result
dynamic_ :: forall e a. Dynamic (Node e a) -> Node e Unit
dynamic_ = void <<< dynamic

-- | render a dynamic value in Node
renderDynamic :: forall e a b. NodeRenderable e a b => Dynamic a -> Node e (Dynamic b)
renderDynamic = dynamic <<< map render
