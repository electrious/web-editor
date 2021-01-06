module Model.HouseBuilder.HouseTop where

import Prelude

import Data.Array (cons)
import Data.Default (class Default)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (%~))
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

-- HouseTop includes all points, lines and faces of the house top part
newtype HouseTop = HouseTop {
    points :: Array HousePoint,
    lines  :: Array HouseLine
    }

derive instance newtypeHouseTop :: Newtype HouseTop _
derive instance genericHouseTop :: Generic HouseTop _
derive instance eqHouseTop :: Eq HouseTop
instance showHouseTop :: Show HouseTop where
    show = genericShow
instance defaultHouseTop :: Default HouseTop where
    def = HouseTop {
        points : [],
        lines  : []
        }

_points :: forall t a r. Newtype t { points :: a | r } => Lens' t a
_points = _Newtype <<< prop (SProxy :: SProxy "points")

_lines :: forall t a r. Newtype t { lines :: a | r } => Lens' t a
_lines = _Newtype <<< prop (SProxy :: SProxy "lines")

addHousePoint :: HousePoint -> HouseTop -> HouseTop
addHousePoint p h = h # _points %~ cons p

addHouseLine :: HouseLine -> HouseTop -> HouseTop
addHouseLine l h = h # _lines %~ cons l
