module SmartHouse.Algorithm.EdgeInfo where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Math.Angle (Angle)
import Math.LineSeg (LineSeg)
import Three.Math.Vector (Vector3)
import Type.Proxy (Proxy(..))

newtype EdgeInfo = EdgeInfo {
    line  :: LineSeg Vector3,
    slope :: Angle
}

derive instance Newtype EdgeInfo _
derive instance Generic EdgeInfo _
instance Show EdgeInfo where
    show = genericShow

_line :: forall t a r. Newtype t { line :: a | r } => Lens' t a
_line = _Newtype <<< prop (Proxy :: Proxy "line")

mkEdgeInfo :: LineSeg Vector3 -> Angle -> EdgeInfo
mkEdgeInfo l s = EdgeInfo { line : l, slope : s }
