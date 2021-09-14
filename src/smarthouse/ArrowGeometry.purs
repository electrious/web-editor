module SmartHouse.ArrowGeometry where

import Prelude

import Effect.Unsafe (unsafePerformEffect)
import Three.Core.Geometry (ShapeGeometry, mkShapeGeometry)
import Three.Math.Path (bezierCurveTo, lineTo, moveTo)
import Three.Math.Shape (Shape, mkShape)


-- arrow shape based on svg file: https://www.svgrepo.com/svg/17993/curve-arrow
-- with the help of svg editor here: https://yqnn.github.io/svg-path-editor/
rotateArrowShape :: Shape
rotateArrowShape = unsafePerformEffect do
    p <- mkShape

    moveTo 197.007 48.479 p
    lineTo 139.0 0.0 p
    lineTo 139.0 28.623 p
    bezierCurveTo 63.505 32.538 3.006 95.472 3.006 172.271 p
    lineTo 43.105 172.271 p
    bezierCurveTo 43.105 117.589 85.632 72.657 139.348 65.801 p
    lineTo 139.348 96.957 p
    lineTo 197.007 48.479 p

    pure p


rotateArrowGeo :: ShapeGeometry
rotateArrowGeo = unsafePerformEffect $ mkShapeGeometry rotateArrowShape
