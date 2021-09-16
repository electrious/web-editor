module Smarthouse.HouseNode where

import Prelude

import Control.Plus (empty)
import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UUIDWrapper (UUID, emptyUUID)
import Editor.Common.Lenses (_id)
import Editor.RoofManager (ArrayEvents)
import FRP.Event (Event)
import Model.SmartHouse.House (House)
import Model.UUID (class HasUUID)
import Models.SmartHouse.ActiveItem (ActHouseItem)
import Type.Proxy (Proxy(..))

-- | Types of operation applied to houses
data HouseOp = HouseOpCreate House
             | HouseOpDelete UUID
             | HouseOpUpdate House

derive instance Generic HouseOp _
derive instance Eq HouseOp

instance Show HouseOp where
    show = genericShow

newtype HouseNode = HouseNode {
    id           :: UUID,
    updated      :: Event HouseOp,
    activated    :: Event UUID,
    actHouseItem :: Event ActHouseItem,
    arrayEvents  :: ArrayEvents
    }

derive instance Newtype HouseNode _
instance HasUUID HouseNode where
    idLens = _id
instance Default HouseNode where
    def = HouseNode {
        id           : emptyUUID,
        updated      : empty,
        activated    : empty,
        actHouseItem : empty,
        arrayEvents  : def
        }

_activated :: forall t a r. Newtype t { activated :: a | r } => Lens' t a
_activated = _Newtype <<< prop (Proxy :: Proxy "activated")

_actHouseItem :: forall t a r. Newtype t { actHouseItem :: a | r } => Lens' t a
_actHouseItem = _Newtype <<< prop (Proxy :: Proxy "actHouseItem")