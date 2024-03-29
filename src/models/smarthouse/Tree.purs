module Model.SmartHouse.Tree where

import Prelude

import Control.Plus (empty)
import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UUIDWrapper (UUID, emptyUUID, genUUID)
import Editor.Common.Lenses (_height, _id, _position)
import Effect (Effect)
import FRP.Event (Event)
import Model.UUID (class HasUUID)
import Three.Math.Vector (Vector3)
import Type.Proxy (Proxy(..))

newtype TreePart = TreePart {
    height :: Meter,
    dia    :: Meter
    }

derive instance Newtype TreePart _
derive instance Generic TreePart _
derive instance Eq TreePart
instance Default TreePart where
    def = TreePart {
        height : def,
        dia    : def
        }
instance Show TreePart where
    show = genericShow


_dia :: forall t a r. Newtype t { dia :: a | r } => Lens' t a
_dia = _Newtype <<< prop (Proxy :: Proxy "dia")

mkTreePart :: Meter -> Meter -> TreePart
mkTreePart h d = TreePart { height: h, dia: d }

newtype Tree = Tree {
    id       :: UUID,
    height   :: Meter,
    crown    :: TreePart,
    barrel   :: TreePart,
    canopy   :: TreePart,

    position :: Vector3
    }


derive instance Newtype Tree _
derive instance Generic Tree _
derive instance Eq Tree
instance HasUUID Tree where
    idLens = _id
instance Show Tree where
    show = genericShow
instance Default Tree where
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
_crown = _Newtype <<< prop (Proxy :: Proxy "crown")

_barrel :: forall t a r. Newtype t { barrel :: a | r } => Lens' t a
_barrel = _Newtype <<< prop (Proxy :: Proxy "barrel")

_canopy :: forall t a r. Newtype t { canopy :: a | r } => Lens' t a
_canopy = _Newtype <<< prop (Proxy :: Proxy "canopy")


mkTree :: Vector3 -> Effect Tree
mkTree p = do
    i <- genUUID
    pure $ def # _id .~ i
               # _position .~ p


data TreeOp = TreeOpCreate Tree
            | TreeOpDelete UUID
            | TreeOpUpdate Tree
derive instance eqTreeOp :: Eq TreeOp

newtype TreeNode = TreeNode {
    id      :: UUID,
    tapped  :: Event UUID,
    updated :: Event TreeOp
}

derive instance Newtype TreeNode _
instance HasUUID TreeNode where
    idLens = _id
instance Default TreeNode where
    def = TreeNode {
        id      : emptyUUID,
        tapped  : empty,
        updated : empty
    }
