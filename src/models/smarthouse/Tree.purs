module Model.SmartHouse.Tree where

import Prelude

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Meter (Meter)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.UUID (UUID)
import Editor.Common.Lenses (_id)
import Model.UUID (class HasUUID)
import Three.Math.Vector (Vector3)

newtype TreePart = TreePart {
    height :: Meter,
    dia    :: Meter
    }

derive instance newtypeTreepart :: Newtype TreePart _
derive instance eqTreePart :: Eq TreePart

_dia :: forall t a r. Newtype t { dia :: a | r } => Lens' t a
_dia = _Newtype <<< prop (SProxy :: SProxy "dia")


newtype Tree = Tree {
    id       :: UUID,
    height   :: Meter,
    crown    :: TreePart,
    barrel   :: TreePart,
    canopy   :: TreePart,

    position :: Vector3
    }


derive instance newtypeTree :: Newtype Tree _
instance hasUUIDTree :: HasUUID Tree where
    idLens = _id

_crown :: forall t a r. Newtype t { crown :: a | r } => Lens' t a
_crown = _Newtype <<< prop (SProxy :: SProxy "crown")

_barrel :: forall t a r. Newtype t { barrel :: a | r } => Lens' t a
_barrel = _Newtype <<< prop (SProxy :: SProxy "barrel")

_canopy :: forall t a r. Newtype t { canopy :: a | r } => Lens' t a
_canopy = _Newtype <<< prop (SProxy :: SProxy "canopy")
