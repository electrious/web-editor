module Editor.EditorMode where

import Prelude

import Control.Monad.Except (except)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (note)
import Data.Enum (class BoundedEnum, class Enum, fromEnum, toEnum)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (singleton)
import Data.Show.Generic (genericShow)
import Foreign.Generic (class Decode, class Encode, decode, encode)
import Foreign.Generic as F

-- | Define the different modes the editor is running in.
data EditorMode = Showing
                | RoofEditing
                | ArrayEditing
                | HouseBuilding

derive instance Generic EditorMode _
derive instance Eq EditorMode
derive instance Ord EditorMode
instance Show EditorMode where
    show = genericShow
instance Bounded EditorMode where
    top = genericTop
    bottom = genericBottom
instance Enum EditorMode where
    succ = genericSucc
    pred = genericPred
instance BoundedEnum EditorMode where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum
instance Encode EditorMode where
    encode = fromEnum >>> encode
instance Decode EditorMode where
    decode = decode >=> toEnum >>> note (singleton $ F.ForeignError "Invalid EditorMode") >>> except