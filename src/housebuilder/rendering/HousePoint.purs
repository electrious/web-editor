module HouseBuilder.Rendering.HousePoint where

import Prelude

import Data.Function.Memoize (memoize)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import HouseBuilder.Rendering.Materials (blueMat, greenMat)
import Model.HouseEditor.HousePoint (HousePoint(..), pointPos)
import Rendering.Renderable (class Renderable)
import Three.Core.Geometry (CircleGeometry, mkCircleGeometry)
import Three.Core.Mesh (Mesh, mkMesh)
import Three.Core.Object3D (setName, setPosition)

newtype HousePointRenderable = HousePointRenderable HousePoint
instance renderableHousePoint :: Renderable HousePointRenderable Mesh where
    render (HousePointRenderable hp) = liftEffect do
        let getMat (HousePointGutter _) = blueMat
            getMat (HousePointRidge _)  = greenMat

        mesh <- mkMesh (geoForPoint unit) (getMat hp)
        setName "house-point" mesh
        setPosition (pointPos hp) mesh
        
        pure mesh

geoForPoint :: Unit -> CircleGeometry
geoForPoint = memoize (\_ -> unsafePerformEffect $ mkCircleGeometry 0.5 32)
