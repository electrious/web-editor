module Model.ActiveMode where

import Data.Bounded (class Ord)
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Show (class Show)

data ActiveMode = Active
                | Inactive

derive instance genericActiveMode :: Generic ActiveMode _
derive instance eqActiveMode :: Eq ActiveMode
derive instance ordActiveMode :: Ord ActiveMode

instance showActiveMode :: Show ActiveMode where
    show = genericShow
