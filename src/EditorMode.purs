module Editor.EditorMode where

import Prelude

import Data.Enum (class BoundedEnum, class Enum)
import Data.Generic.Rep (class Generic)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Show.Generic (genericShow)

-- | Define the different modes the editor is running in.
data EditorMode = Showing
                | RoofEditing
                | ArrayEditing
                | HouseBuilding

derive instance genericEditorMode :: Generic EditorMode _
derive instance eqEditorMode :: Eq EditorMode
derive instance ordEditorMode :: Ord EditorMode
instance showEditorMode :: Show EditorMode where
    show = genericShow
instance boundEditorMode :: Bounded EditorMode where
    top = genericTop
    bottom = genericBottom
instance enumEditorMode :: Enum EditorMode where
    succ = genericSucc
    pred = genericPred
instance boundEnumEditorMode :: BoundedEnum EditorMode where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum
