module Model.HouseBuilder.HouseTop where

import Prelude

import Data.Default (class Default)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Model.HouseBuilder.Gutter (Gutter)
import Model.HouseBuilder.HousePoint (HousePoint)
import Model.HouseBuilder.Ridge (Ridge)

-- HouseLine is either a ridge line or a gutter line
data HouseLine = HLRidge Ridge
               | HLGutter Gutter

derive instance genericHouseLine :: Generic HouseLine _
derive instance eqHouseLine :: Eq HouseLine
instance showHouseLine :: Show HouseLine where
    show = genericShow

-- HouseFace is Triangle with indices to house points
newtype HouseFace = HouseFace {
    idxA :: Int,
    idxB :: Int,
    idxC :: Int
    }

derive instance newtypeHouseFace :: Newtype HouseFace _
derive instance genericHouseFace :: Generic HouseFace _
derive instance eqHouseFace :: Eq HouseFace
instance showHouseFace :: Show HouseFace where
    show = genericShow
instance defaultHouseFace :: Default HouseFace where
    def = HouseFace {
        idxA : 0,
        idxB : 0,
        idxC : 0
        }

_idxA :: forall t a r. Newtype t { idxA :: a | r } => Lens' t a
_idxA = _Newtype <<< prop (SProxy :: SProxy "idxA")

_idxB :: forall t a r. Newtype t { idxB :: a | r } => Lens' t a
_idxB = _Newtype <<< prop (SProxy :: SProxy "idxB")

_idxC :: forall t a r. Newtype t { idxC :: a | r } => Lens' t a
_idxC = _Newtype <<< prop (SProxy :: SProxy "idxC")

-- HouseTop includes all points, lines and faces of the house top part
newtype HouseTop = HouseTop {
    points :: Array HousePoint,
    lines  :: Array HouseLine,
    faces  :: Array HouseFace
    }

derive instance newtypeHouseTop :: Newtype HouseTop _
derive instance genericHouseTop :: Generic HouseTop _
derive instance eqHouseTop :: Eq HouseTop
instance showHouseTop :: Show HouseTop where
    show = genericShow
instance defaultHouseTop :: Default HouseTop where
    def = HouseTop {
        points : [],
        lines  : [],
        faces  : []
        }

_points :: forall t a r. Newtype t { points :: a | r } => Lens' t a
_points = _Newtype <<< prop (SProxy :: SProxy "points")

_gutters :: forall t a r. Newtype t { gutters :: a | r } => Lens' t a
_gutters = _Newtype <<< prop (SProxy :: SProxy "gutters")

_faces :: forall t a r . Newtype t { faces :: a | r } => Lens' t a
_faces = _Newtype <<< prop (SProxy :: SProxy "faces")

