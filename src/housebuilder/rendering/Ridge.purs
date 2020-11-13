module HouseBuilder.Rendering.Ridge where

import Prelude hiding (add)

import Data.Lens ((^.))
import Effect (Effect)
import Effect.Class (liftEffect)
import HouseBuilder.Rendering.HousePoint (HousePointRenderable(..))
import Model.HouseBuilder.Ridge (Ridge, _point1, _point2)
import Model.HouseEditor.HousePoint (pointPos)
import Rendering.Renderable (class Renderable, render)
import Three.Core.Geometry (mkLineGeometry, setLinePositions)
import Three.Core.Material (mkLineBasicMaterial)
import Three.Core.Mesh (Line2, Mesh, mkLine2)
import Three.Core.Object3D (Object3D, add, mkObject3D, setName)
import Three.Math.Vector (Vector3)

newtype RidgeRenderable = RidgeRenderable Ridge
instance renderableRidge :: Renderable RidgeRenderable Object3D where
    render (RidgeRenderable r) = do
        ro <- liftEffect mkObject3D
        liftEffect $ setName "Ridge" ro

        -- render the points
        p1 :: Mesh <- render $ HousePointRenderable $ r ^. _point1
        p2 :: Mesh <- render $ HousePointRenderable $ r ^. _point2

        liftEffect $ add p1 ro
        liftEffect $ add p2 ro

        -- render the ridge line
        let v1 = pointPos $ r ^. _point1
            v2 = pointPos $ r ^. _point2
        line <- liftEffect $ renderLine v1 v2
        liftEffect $ add line ro

        pure ro


renderLine :: Vector3 -> Vector3 -> Effect Line2
renderLine p1 p2 = do
    -- setup the line geometry
    geo <- mkLineGeometry
    setLinePositions [p1, p2] geo
    --setLineColors [mkColorString "white"] geo

    mat <- mkLineBasicMaterial 0xffffff 2.0

    l <- mkLine2 geo mat
    setName "line" l
    pure l
