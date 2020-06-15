module Data.Default where

import Prelude

-- | class to represent types that have a default value
class Default a where
    def :: a

instance defaultUNit :: Default Unit where
    def = unit