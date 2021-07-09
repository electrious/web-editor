module Model.ActiveMode where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data ActiveMode = Active
                | Inactive

derive instance genericActiveMode :: Generic ActiveMode _
derive instance eqActiveMode :: Eq ActiveMode
derive instance ordActiveMode :: Ord ActiveMode
instance showActiveMode :: Show ActiveMode where
    show = genericShow
instance heytingActiveMode :: HeytingAlgebra ActiveMode where
    tt = Active
    ff = Inactive
    implies a b = not a || b
    
    conj Active Active = Active
    conj _ Inactive    = Inactive
    conj Inactive _    = Inactive

    disj Active _          = Active
    disj _ Active          = Active
    disj Inactive Inactive = Inactive

    not Active   = Inactive
    not Inactive = Active

isActive :: ActiveMode -> Boolean
isActive = (==) Active

toBoolean :: ActiveMode -> Boolean
toBoolean = isActive

fromBoolean :: Boolean -> ActiveMode
fromBoolean true  = Active
fromBoolean false = Inactive
