module Editor.PanelOperation where

import Data.List (List)
import Data.UUID (UUID)
import Model.Roof.Panel (Panel)
import Three.Math.Vector (Vector3)

-- | Panel operation that change a panel array.
data PanelOperation = AddPanel Panel
                    | AddPanels (List Panel)
                    | TempPanels (List Panel)
                    | DelPanel UUID
                    | DeleteAll
                    | UpdatePanel Panel
                    | MoveArray Int Vector3
                    | PreserveTempPanels
