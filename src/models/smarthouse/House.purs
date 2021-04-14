module Model.SmartHouse.House where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List, singleton)
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.UUID (UUID, genUUID)
import Editor.Common.Lenses (_id)
import Effect (Effect)
import FRP.Event (Event)
import Math.Angle (Angle)
import Model.Polygon (Polygon, counterClockPoly)
import Model.SmartHouse.Roof (Roof, createRoofFrom)
import Model.UUID (class HasUUID, idLens)
import SmartHouse.Algorithm.Skeleton (skeletonize)
import Three.Math.Vector (Vector3)

newtype House = House {
    id     :: UUID,
    floor  :: Polygon Vector3,
    height :: Meter,
    roofs  :: List Roof
    }

derive instance newtypeHouse :: Newtype House _
derive instance genericHouse :: Generic House _
instance eqHouse :: Eq House where
    eq h1 h2 = h1 ^. _id == h2 ^. _id
instance showHouse :: Show House where
    show = genericShow
instance hasUUIDHouse :: HasUUID House where
    idLens = _id

createHouseFrom :: Angle -> Polygon Vector3 -> Effect House
createHouseFrom slope poly = do
    i <- genUUID
    roofPolys <- skeletonize slope $ singleton $ counterClockPoly poly
    roofs <- traverse createRoofFrom roofPolys
    
    pure $ House {
        id     : i,
        floor  : poly,
        height : meter 3.5,   -- default height
        roofs  : roofs
        }

newtype HouseNode = HouseNode {
    id         :: UUID,
    roofTapped :: Event UUID,
    wallTapped :: Event Unit
    }

derive instance newtypeHouseNode :: Newtype HouseNode _
instance hasUUIDHouseNode :: HasUUID HouseNode where
    idLens = _id

_roofTapped :: forall t a r. Newtype t { roofTapped :: a | r } => Lens' t a
_roofTapped = _Newtype <<< prop (SProxy :: SProxy "roofTapped")

_wallTapped :: forall t a r. Newtype t { wallTapped :: a | r } => Lens' t a
_wallTapped = _Newtype <<< prop (SProxy :: SProxy "wallTapped")

houseTapped :: HouseNode -> Event UUID
houseTapped h = (const i <$> h ^. _roofTapped) <|> (const i <$> h ^. _wallTapped)
    where i = h ^. idLens
