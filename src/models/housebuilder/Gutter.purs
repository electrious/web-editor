module Model.HouseBuilder.Gutter (Gutter, gutter, gutterLine) where

import Data.Lens ((^.))
import Math.Line (Line, mkLine)
import Model.HouseBuilder.HousePoint (GutterPoint, _gutterPointPos)

data Gutter = Gutter GutterPoint GutterPoint

gutter :: GutterPoint -> GutterPoint -> Gutter
gutter = Gutter

gutterLine :: Gutter -> Line
gutterLine (Gutter g1 g2) = mkLine (g1 ^. _gutterPointPos) (g2 ^. _gutterPointPos)
