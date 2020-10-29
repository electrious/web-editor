module Model.HouseBuilder.House where

import Prelude
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Model.HouseBuilder.Ridge (Ridge)
import Model.Roof.RoofPlate (RoofEdited)

newtype House = House {
    ridges :: Array Ridge,
    roofs  :: Array RoofEdited
}

derive instance newtypeHouse :: Newtype House _

_ridges :: forall t a r. Newtype t { ridges :: a | r } => Lens' t a
_ridges = _Newtype <<< prop (SProxy :: SProxy "ridges")
