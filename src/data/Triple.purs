module Data.Triple where

data Triple a b c = Triple a b c

first :: forall a b c. Triple a b c -> a
first (Triple a _ _) = a

second :: forall a b c. Triple a b c -> b
second (Triple _ b _) = b

third :: forall a b c. Triple a b c -> c
third (Triple _ _ c) = c
