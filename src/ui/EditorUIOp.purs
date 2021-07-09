module UI.EditorUIOp where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Show (class Show)

data EditorUIOp = RoofSaved
                | ArraySaved
                | HouseSaved
                | Close

derive instance genericEditorUIOp :: Generic EditorUIOp _
derive instance eqEditorUIOp :: Eq EditorUIOp
instance showEditorUIOp :: Show EditorUIOp where
    show = genericShow
