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
import Data.UUID (UUID, emptyUUID)
import Editor.Common.Lenses (_id)
import Editor.RoofManager (ArrayEvents)
import FRP.Event (Event)
import Model.SmartHouse.House (House)
import Model.UUID (class HasUUID)
import Models.SmartHouse.ActiveItem (ActHouseRoof)
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
    actHouseRoof :: Event ActHouseRoof,
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
        actHouseRoof : empty,
        arrayEvents  : def
        }

_activated :: forall t a r. Newtype t { activated :: a | r } => Lens' t a
_activated = _Newtype <<< prop (Proxy :: Proxy "activated")

_actHouseRoof :: forall t a r. Newtype t { actHouseRoof :: a | r } => Lens' t a
_actHouseRoof = _Newtype <<< prop (Proxy :: Proxy "actHouseRoof")