module Editor.Disposable where

import Prelude
import Data.Foldable (class Foldable, traverse_)
import Data.Newtype (class Newtype)
import Effect (Effect)

-- | Disposable represents values that can be disposed
class Disposable d where
    dispose :: d -> Effect Unit


disposeAll :: forall d f. Foldable f => Disposable d => f d -> Effect Unit
disposeAll = traverse_ dispose

-- Value defined to dispose any Disposable value
newtype Disposee = Disposee (Effect Unit)

derive instance newtypeDisposee :: Newtype Disposee _
instance disposableDisposee :: Disposable Disposee where
    dispose (Disposee d) = d
derive newtype instance semigroupDisposee :: Semigroup Disposee
derive newtype instance monoidDisposee :: Monoid Disposee

toDisposee :: forall a. Disposable a => a -> Disposee
toDisposee = Disposee <<< dispose
