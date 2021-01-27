module Data.Default where

import Prelude

-- | class to represent types that have a default value
class Default a where
    def :: a

instance defaultInt :: Default Int where
    def = 0
instance defaultNumber :: Default Number where
    def = 0.0
instance defaultUnit :: Default Unit where
    def = unit
