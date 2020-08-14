module Editor.PanelOperation where

import Data.List (List)
import Data.UUID (UUID)
import Model.Roof.Panel (Panel)
import Three.Math.Vector (Vector3)

-- | Panel operation that change a panel array.
data PanelOperation = AddPanel Panel
                    | AddPanels (List Panel)
                    | DelPanel UUID
                    | DelPanels (List UUID)
                    | DeleteAll
                    | UpdatePanels (List Panel)


-- | operation to update panels, or temp panels, or move rendered panels only.
data ArrayOperation = PanelOperation PanelOperation
                    | TempPanels (List Panel)
                    | MoveArray Int Vector3
                    | PreserveTempPanels
