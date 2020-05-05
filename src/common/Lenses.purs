module Editor.Common.Lenses where

import Prelude

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))

_leadId :: forall t a r. Newtype t { leadId :: a | r } => Lens' t a
_leadId = _Newtype <<< prop (SProxy :: SProxy "leadId")

_disposable :: forall t a r. Newtype t { disposable :: a | r } => Lens' t a
_disposable = _Newtype <<< prop (SProxy :: SProxy "disposable")

_tapped :: forall t a r. Newtype t { tapped :: a | r } => Lens' t a
_tapped = _Newtype <<< prop (SProxy :: SProxy "tapped")

_id :: forall t a r. Newtype t { id :: a | r } => Lens' t a
_id = _Newtype <<< prop (SProxy :: SProxy "id")

_roofId :: forall t a r. Newtype t { roofId :: a | r } => Lens' t a
_roofId = _Newtype <<< prop (SProxy :: SProxy "roofId")

_roofs :: forall t a r. Newtype t { roofs :: a | r } => Lens' t a
_roofs = _Newtype <<< prop (SProxy :: SProxy "roofs")

_center :: forall t a r. Newtype t { center :: a | r } => Lens' t a
_center = _Newtype <<< prop (SProxy :: SProxy "center")

_normal :: forall t a r. Newtype t { normal :: a | r } => Lens' t a
_normal = _Newtype <<< prop (SProxy :: SProxy "normal")

_slope :: forall t a r. Newtype t { slope :: a | r } => Lens' t a
_slope = _Newtype <<< prop (SProxy :: SProxy "slope")

_polygon :: forall t a r. Newtype t { polygon :: a | r } => Lens' t a
_polygon = _Newtype <<< prop (SProxy :: SProxy "polygon")

_orientation :: forall t a r. Newtype t { orientation :: a | r } => Lens' t a
_orientation = _Newtype <<< prop (SProxy :: SProxy "orientation")

_alignment :: forall t a r. Newtype t { alignment :: a | r } => Lens' t a
_alignment = _Newtype <<< prop (SProxy :: SProxy "alignment")

_x :: forall t a r. Newtype t { x :: a | r } => Lens' t a
_x = _Newtype <<< prop (SProxy :: SProxy "x")

_y :: forall t a r. Newtype t { y :: a | r } => Lens' t a
_y = _Newtype <<< prop (SProxy :: SProxy "y")

_z :: forall t a r. Newtype t { z :: a | r } => Lens' t a
_z = _Newtype <<< prop (SProxy :: SProxy "z")