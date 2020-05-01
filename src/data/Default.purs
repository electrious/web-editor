module Data.Default where

-- | class to represent types that have a default value
class Default a where
    def :: a
