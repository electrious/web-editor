module Editor.EditorMode where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Enum (class BoundedEnum, class Enum, fromEnum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Show.Generic (genericShow)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Foreign.Generic (class Decode, class Encode, ForeignError(..), decode, encode)

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
instance encodeEditorMode :: Encode EditorMode where
    encode = fromEnum >>> encode
instance decodeEditorMode :: Decode EditorMode where
    decode o = decode o >>= \i -> do
                case toEnum i of
                    Just v -> pure v
                    Nothing -> throwError $ singleton $ ForeignError $ "Can't decode EditorMode from: " <> show i
