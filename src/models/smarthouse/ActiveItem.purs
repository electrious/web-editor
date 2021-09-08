module Models.SmartHouse.ActiveItem where

import Data.Default (class Default, def)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_chimney)
import Model.SmartHouse.Chimney (Chimney)
import Model.SmartHouse.House (House)
import Model.SmartHouse.Roof (Roof)
import Model.SmartHouse.Tree (Tree)


newtype ActHouseItem = ActHouseItem {
    house   :: House,
    roof    :: Maybe Roof,
    chimney :: Maybe Chimney
}

derive instance Newtype ActHouseItem _
instance Default ActHouseItem where
    def = ActHouseItem {
        house   : def,
        roof    : Nothing,
        chimney : Nothing
    }

data ActiveItem = ActiveHouse ActHouseItem
                | ActiveTree Tree

isActiveHouse :: ActiveItem -> Boolean
isActiveHouse (ActiveHouse h) = isNothing (h ^. _chimney)
isActiveHouse (ActiveTree _)  = false

activeHouse :: ActiveItem -> Maybe ActHouseItem
activeHouse (ActiveHouse h) = Just h
activeHouse (ActiveTree _)  = Nothing
