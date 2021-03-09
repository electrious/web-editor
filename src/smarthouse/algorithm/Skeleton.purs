module SmartHouse.Algorithm.Skeleton where

import Data.Eq (class Eq)
import Three.Math.Vector (Vector3)


newtype EdgeEvent = EdgeE {
    distance :: Number,
    intersection :: Vector3    
}

data PointEvent = EdgeEvent
                | SplitEvent
                  
derive instance eqPointEvent :: Eq PointEvent
