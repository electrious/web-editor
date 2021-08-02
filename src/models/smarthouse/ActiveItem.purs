module Models.SmartHouse.ActiveItem where

import Data.Default (class Default, def)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_roof)
import Model.SmartHouse.House (House)
import Model.SmartHouse.Roof (Roof)
import Model.SmartHouse.Tree (Tree)


newtype ActHouseRoof = ActHouseRoof {
    house :: House,
    roof  :: Maybe Roof
}

derive instance Newtype ActHouseRoof _
instance Default ActHouseRoof where
    def = ActHouseRoof {
        house : def,
        roof  : Nothing
    }

data ActiveItem = ActiveHouse ActHouseRoof
                | ActiveTree Tree


activeRoof :: ActiveItem -> Maybe Roof
activeRoof (ActiveHouse h) = h ^. _roof
activeRoof (ActiveTree _)  = Nothing
