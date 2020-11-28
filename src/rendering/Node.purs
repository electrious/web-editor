module Rendering.Node where

import Prelude hiding (add)

import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, local, runReaderT)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Three.Core.Object3D (Object3D, add, mkObject3D, setName)

newtype Node a = Node (ReaderT Object3D Effect a)

derive instance newtypeNode :: Newtype (Node a) _

derive newtype instance functorNode :: Functor Node
derive newtype instance applyNode :: Apply Node
derive newtype instance applicativeNode :: Applicative Node
derive newtype instance bindNode :: Bind Node
derive newtype instance monadNode :: Monad Node
derive newtype instance monadEffectNode :: MonadEffect Node
derive newtype instance monadAskNode :: MonadAsk Object3D Node
derive newtype instance monadReaderNode :: MonadReader Object3D Node

runNode :: forall a. Node a -> Object3D -> Effect a
runNode (Node r) = runReaderT r


node :: forall a. String -> Node a -> Node a
node name child = do
    w <- liftEffect mkObject3D
    liftEffect $ setName name w
    
    parent <- ask
    liftEffect $ add w parent

    -- run child action with the new parent node
    local (const w) child
