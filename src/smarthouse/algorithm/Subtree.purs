module Smarthouse.Algorithm.Subtree where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.Newtype (class Newtype)
import Data.Show (class Show)
import Three.Math.Vector (Vector3)

newtype Subtree = Subtree {
    source :: Vector3,
    height :: Number,
    sinks  :: List Vector3
    }

derive instance newtypeSubtree :: Newtype Subtree _
derive instance genericSubtree :: Generic Subtree _
instance showSubtree :: Show Subtree where
    show = genericShow

subtree :: Vector3 -> Number -> List Vector3 -> Subtree
subtree source h ss = Subtree {
    source : source,
    height : h,
    sinks  : ss
    }
