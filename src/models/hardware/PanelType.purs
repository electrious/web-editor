module Model.Hardware.PanelType where

import Prelude

import Data.Enum (class BoundedEnum, class Enum)
import Data.Function.Memoize (class Tabulate, genericTabulate)
import Data.Generic.Rep (class Generic)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum.Generic (genericCardinality, genericPred, genericSucc)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..))

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
instance tabulatePanelType :: Tabulate PanelType where
    tabulate = genericTabulate
