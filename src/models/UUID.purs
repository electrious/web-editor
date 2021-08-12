module Model.UUID where

import Prelude

import Data.Lens (Lens', (.~))
import Data.Traversable (class Traversable, traverse)
import Data.UUID (UUID, genUUID)
import Effect (Effect)

class HasUUID a where
    idLens :: Lens' a UUID


assignNewId :: forall a. HasUUID a => a -> Effect a
assignNewId a = do
    i <- genUUID
    pure $ a # idLens .~ i

assignNewIds :: forall a t. HasUUID a => Traversable t => t a -> Effect (t a)
assignNewIds = traverse assignNewId