module Rendering.Line where

import Prelude

import Data.Array (fromFoldable)
import Data.Default (def)
import Data.Lens ((.~), (^.))
import Data.Meter (feetInchStr, meter)
import Data.Traversable (class Traversable, traverse_)
import Editor.Common.Lenses (_name, _position)
import Effect.Unsafe (unsafePerformEffect)
import Math.LineSeg (LineSeg, _end, _start, length, lineSegsFromPolyVerts, lineVec)
import Rendering.Node (Node, _fontSize, line, node, text3D)
import Three.Core.Material (LineBasicMaterial, mkLineBasicMaterial)
import Three.Math.Vector (Vector3, mkVec3, vecX, vecY, vecZ, (<**>), (<+>))



moveLeft :: Number -> Vector3 -> Vector3
moveLeft d v = mkVec3 (vecX v - d) (vecY v) (vecZ v)

moveUp :: Number -> Vector3 -> Vector3
moveUp dh v = mkVec3 (vecX v) (vecY v) (vecZ v + dh)

moveAway :: Number -> Vector3 -> Vector3
moveAway d v = mkVec3 (vecX v) (vecY v + d) (vecZ v)

lineMat :: LineBasicMaterial
lineMat = unsafePerformEffect $ mkLineBasicMaterial 0xeeeeee 4.0

renderLine :: forall e. LineSeg Vector3 -> Node e Unit
renderLine l = renderLineWith l lineMat

renderLineWith :: forall e. LineSeg Vector3 -> LineBasicMaterial -> Node e Unit
renderLineWith l mat = do
    let vs = [l ^. _start, l ^. _end]
    void $ line (def # _name .~ "line") vs mat
    renderLineLength l

renderPolyLine :: forall e f. Traversable f => f Vector3 -> Node e Unit
renderPolyLine vs = do
    void $ line (def # _name .~ "polygon-line") (fromFoldable vs) lineMat
    traverse_ renderLineLength $ lineSegsFromPolyVerts vs

-- render line length text
renderLineLength :: forall e. LineSeg Vector3 -> Node e Unit
renderLineLength l = do
    let tPos = moveUp 0.1 $ (l ^. _start <+> l ^. _end) <**> 0.5
        lv = lineVec l

        rPos = if vecY lv < 0.0
               then moveLeft 3.0 tPos
               else if vecX lv < 0.0
                    then moveAway 2.0 tPos
                    else tPos

        lStr = feetInchStr $ meter $ length l
    void $ node (def # _position .~ pure rPos) $ text3D (def # _fontSize .~ 1.0) lStr