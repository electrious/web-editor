module Model.HouseBuilder.Ridge where


import Prelude

import Data.Default (def)
import Data.Lens ((^.))
import Effect.Unsafe (unsafePerformEffect)
import Math.Line (Line, linePoints, mkLine)
import Model.HouseEditor.HousePoint (GutterPoint, RidgePoint, _gutterPointPos, _ridgePointPos)
import Rendering.Node (line)
import Rendering.NodeRenderable (class NodeRenderable)
import Three.Core.Material (LineBasicMaterial, mkLineBasicMaterial)
import Three.Core.Mesh (Line2)


data Ridge = TopRidge  RidgePoint RidgePoint
           | SideRidge RidgePoint GutterPoint


topRidge :: RidgePoint -> RidgePoint -> Ridge
topRidge p1 p2 = TopRidge p1 p2

sideRidge :: RidgePoint -> GutterPoint -> Ridge
sideRidge p1 p2 = SideRidge p1 p2

ridgeLine :: Ridge -> Line
ridgeLine (TopRidge p1 p2)  = mkLine (p1 ^. _ridgePointPos) (p2 ^. _ridgePointPos)
ridgeLine (SideRidge p1 p2) = mkLine (p1 ^. _ridgePointPos) (p2 ^. _gutterPointPos)

ridgeLineMat :: LineBasicMaterial
ridgeLineMat = unsafePerformEffect $ mkLineBasicMaterial 0x333333 0.5
    
instance nodeRenderableRidge :: NodeRenderable e Ridge Line2 where
    render r = line def (linePoints $ ridgeLine r) ridgeLineMat
