module Math.Line where

import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Prelude ((<<<))
import Three.Math.Vector (Vector3)

newtype Line = Line {
  start :: Vector3,
  end   :: Vector3
  }

derive instance newtypeLine :: Newtype Line _
derive instance genericLine :: Generic Line _

_start :: forall t a r. Newtype t { start :: a | r } => Lens' t a
_start = _Newtype <<< prop (SProxy :: SProxy "start")

_end :: forall t a r. Newtype t { end :: a | r } => Lens' t a
_end = _Newtype <<< prop (SProxy :: SProxy "end")

mkLine :: Vector3 -> Vector3 -> Line
mkLine v1 v2 = Line { start : v1, end : v2 }
