module Editor.Disposable where

import Prelude

import Data.Foldable (class Foldable, traverse_)
import Effect (Effect)

-- | Disposable represents values that can be disposed
class Disposable d where
    dispose :: d -> Effect Unit


disposeAll :: forall d f. Foldable f => Disposable d => f d -> Effect Unit
disposeAll = traverse_ dispose