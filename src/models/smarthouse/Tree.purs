module Model.SmartHouse.Tree where

import Prelude

import Data.Default (class Default, def)
import Data.Lens (Lens', (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.UUID (UUID, emptyUUID, genUUID)
import Editor.Common.Lenses (_height, _id, _position)
import Effect (Effect)
import Model.UUID (class HasUUID)
import Three.Math.Vector (Vector3)

newtype TreePart = TreePart {
    height :: Meter,
    dia    :: Meter
    }

derive instance newtypeTreepart :: Newtype TreePart _
derive instance eqTreePart :: Eq TreePart
instance defaultTreePart :: Default TreePart where
    def = TreePart {
        height : def,
        dia    : def
        }

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
instance defaultTree :: Default Tree where
    def = Tree {
        id       : emptyUUID,
        height   : meter 10.0,
        crown    : def # _height .~ meter 8.0
                       # _dia    .~ meter 1.0,
        barrel   : def # _height .~ meter 5.0
                       # _dia .~ meter 2.0,
        canopy   : def # _height .~ meter 3.0
                       # _dia .~ meter 1.0,

        position : def
        }

_crown :: forall t a r. Newtype t { crown :: a | r } => Lens' t a
_crown = _Newtype <<< prop (SProxy :: SProxy "crown")

_barrel :: forall t a r. Newtype t { barrel :: a | r } => Lens' t a
_barrel = _Newtype <<< prop (SProxy :: SProxy "barrel")

_canopy :: forall t a r. Newtype t { canopy :: a | r } => Lens' t a
_canopy = _Newtype <<< prop (SProxy :: SProxy "canopy")


mkTree :: Vector3 -> Effect Tree
mkTree p = do
    i <- genUUID
    pure $ def # _id .~ i
               # _position .~ p
