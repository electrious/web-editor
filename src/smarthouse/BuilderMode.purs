module SmartHouse.BuilderMode where

import Prelude

data BuilderMode = Building
                 | Showing

derive instance eqBuilderMode :: Eq BuilderMode

isBuilding :: BuilderMode -> Boolean
isBuilding = (==) Building
