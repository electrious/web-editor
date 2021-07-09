module Model.Hardware.PanelType where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Enum (class BoundedEnum, class Enum, fromEnum, toEnum)
import Data.Function.Memoize (class Tabulate, genericTabulate)
import Data.Generic.Rep (class Generic)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum.Generic (genericCardinality, genericPred, genericSucc)
import Data.Show.Generic (genericShow)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Foreign.Generic (class Decode, class Encode, ForeignError(..), decode, encode)

data PanelType = Premium
               | Standard

derive instance genericPanelType :: Generic PanelType _
derive instance eqPanelType :: Eq PanelType
derive instance ordPanelType :: Ord PanelType
instance showPanelType :: Show PanelType where
    show = genericShow
instance boundPanelTYpe :: Bounded PanelType where
    top = genericTop
    bottom = genericBottom
instance enumPanelType :: Enum PanelType where
    succ = genericSucc
    pred = genericPred
instance boundEnumPanelType :: BoundedEnum PanelType where
    cardinality = genericCardinality

    toEnum 1 = Just Premium
    toEnum 2 = Just Standard
    toEnum _ = Nothing

    fromEnum Premium  = 1
    fromEnum Standard = 2
instance encodePanelType :: Encode PanelType where
    encode = fromEnum >>> encode
instance decodePanelType :: Decode PanelType where
    decode o = decode o >>= \i -> do
                    case toEnum i of
                        Just v -> pure v
                        Nothing -> throwError $ singleton $ ForeignError ("Can't decode PanelType from: " <> show i)
instance tabulatePanelType :: Tabulate PanelType where
    tabulate = genericTabulate
