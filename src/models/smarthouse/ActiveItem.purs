module Models.SmartHouse.ActiveItem where

import Model.SmartHouse.Roof (Roof)
import Model.SmartHouse.Tree (Tree)

data ActiveItem = ActiveRoof Roof
                | ActiveTree Tree
