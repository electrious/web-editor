module Model.HouseBuilder.Gutter (Gutter, gutter, gutterLine) where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((^.))
import Data.Show (class Show)
import Math.Line (Line, mkLine)
import Model.HouseBuilder.HousePoint (GutterPoint, _gutterPointPos)

data Gutter = Gutter GutterPoint GutterPoint

derive instance genericGutter :: Generic Gutter _
derive instance eqGutter :: Eq Gutter
instance showGutter :: Show Gutter where
    show = genericShow

gutter :: GutterPoint -> GutterPoint -> Gutter
gutter = Gutter

gutterLine :: Gutter -> Line
gutterLine (Gutter g1 g2) = mkLine (g1 ^. _gutterPointPos) (g2 ^. _gutterPointPos)
