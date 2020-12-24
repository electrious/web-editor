module HouseBuilder.Rendering.HousePoint (HousePointRenderable(..)) where

import Prelude

import Data.Function.Memoize (memoize)
import Data.Lens ((^.))
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import HouseBuilder.Rendering.Materials (blueMat, greenMat, redMat)
import Model.HouseEditor.HousePoint (GutterPointType(..), HousePoint(..), _pointType, pointPos)
import Rendering.Renderable (class Renderable)
import Three.Core.Geometry (CircleGeometry, mkCircleGeometry)
import Three.Core.Mesh (Mesh, mkMesh)
import Three.Core.Object3D (setName, setPosition)

newtype HousePointRenderable = HousePointRenderable HousePoint
instance renderableHousePoint :: Renderable e HousePointRenderable Mesh where
    render (HousePointRenderable hp) = liftEffect do
        let getMat (HousePointGutter p) = if p ^. _pointType == GPLocked
                                          then blueMat
                                          else redMat
            getMat (HousePointRidge _)  = greenMat

        mesh <- mkMesh (geoForPoint unit) (getMat hp)
        setName (getName hp) mesh
        setPosition (pointPos hp) mesh
        
        pure mesh

geoForPoint :: Unit -> CircleGeometry
geoForPoint = memoize (\_ -> unsafePerformEffect $ mkCircleGeometry 0.5 32)

getName :: HousePoint -> String
getName (HousePointGutter _) = "gutter-point"
getName (HousePointRidge _)  = "ridge-point"
