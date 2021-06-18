module Model.ShadePoint where

import Prelude

import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_rating, _x, _y, _z)
import Model.Roof.RoofPlate (UnifiedPoint)
import Three.Math.Vector (Vector3, mkVec3)

newtype ShadePoint = ShadePoint {
    position  :: Vector3,
    intensity :: Number
}

derive instance newtypeShade :: Newtype ShadePoint _

_intensity :: forall t a r. Newtype t { intensity :: a | r } => Lens' t a
_intensity = _Newtype <<< prop (SProxy :: SProxy "intensity")

mkShadePoint :: Vector3 -> Number -> ShadePoint
mkShadePoint p i = ShadePoint { position : p, intensity : i }

shadePointFrom :: UnifiedPoint -> ShadePoint
shadePointFrom p = mkShadePoint pos i
    where pos = mkVec3 (p ^. _x) (p ^. _y) (p ^. _z)
          i   = 1.0 - p ^. _rating / 100.0
