module Model.ActiveMode where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data ActiveMode = Active
                | Inactive

derive instance genericActiveMode :: Generic ActiveMode _
derive instance eqActiveMode :: Eq ActiveMode
derive instance ordActiveMode :: Ord ActiveMode

instance showActiveMode :: Show ActiveMode where
    show = genericShow

isActive :: ActiveMode -> Boolean
isActive = (==) Active
