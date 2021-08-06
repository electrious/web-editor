module Models.SmartHouse.ActiveItem where

import Data.Default (class Default, def)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
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


isActiveHouse :: ActiveItem -> Boolean
isActiveHouse (ActiveHouse _) = true
isActiveHouse (ActiveTree _)  = false

activeHouse :: ActiveItem -> Maybe ActHouseRoof
activeHouse (ActiveHouse h) = Just h
activeHouse (ActiveTree _)  = Nothing
