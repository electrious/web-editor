module Editor.PanelOperation where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.UUID (UUID)
import Model.Roof.Panel (Panel)
import Three.Math.Vector (Vector3)

-- | Panel operation that change a panel array.
data PanelOperation = LoadPanels (List Panel)
                    | AddPanel Panel
                    | AddPanels (List Panel)
                    | DelPanel UUID
                    | DelPanels (List UUID)
                    | DeleteAll
                    | UpdatePanels (List Panel)

derive instance genericPanelOperation :: Generic PanelOperation _
instance showPanelOperation :: Show PanelOperation where
    show = genericShow

-- | operation to update panels, or temp panels, or move rendered panels only.
data ArrayOperation = PanelOperation PanelOperation
                    | TempPanels (List Panel)
                    | MoveArray Int Vector3
                    | PreserveTempPanels

derive instance genericArrayOperation :: Generic ArrayOperation _
instance showArrayOperation :: Show ArrayOperation where
    show = genericShow
