module Rendering.Racking.XRRendering where

import Prelude hiding (add)

import Data.Lens ((^.))
import Data.Meter (inch, meterVal)
import Data.Traversable (traverse, traverse_)
import Editor.Common.Lenses (_clamps, _flashes, _length, _lfeet, _rails, _splices, _stoppers, _x, _y, _z)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Math (pi)
import Model.Racking.XR10.Clamp (Clamp)
import Model.Racking.XR10.LFoot (LFoot)
import Model.Racking.XR10.Rail (Rail)
import Model.Racking.XR10.Splice (Splice)
import Model.Racking.XR10.Stopper (Stopper)
import Model.Racking.XR10.XRRackingComponent (XRRackingComponent)
import Rendering.Racking.Common (FlashRenderable(..), buildClamp)
import Rendering.Renderable (class Renderable, render)
import Renderring.MaterialLoader (blackMaterial)
import Three.Core.Geometry (BoxGeometry, CylinderGeometry, mkBoxGeometry, mkCylinderGeometry)
import Three.Core.Mesh (Mesh, mkMesh)
import Three.Core.Object3D (Object3D, add, mkObject3D, setName, setPosition, setRotation, setScale)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (mkVec3)

newtype XRRackingComponentRenderable = XRRackingComponentRenderable XRRackingComponent
instance renderableXRRackingComponent :: Renderable e XRRackingComponentRenderable Object3D where
    render (XRRackingComponentRenderable x) = do
        comp <- liftEffect mkObject3D
        liftEffect $ setName "XRRackingComponent" comp

        flashes :: Array Mesh <- traverse render (FlashRenderable <$> x ^. _flashes)
        liftEffect $ traverse_ (flip add comp) flashes

        rails :: Array Mesh <- traverse render (RailRenderable <$> x ^. _rails)
        liftEffect $ traverse_ (flip add comp) rails

        splices :: Array Mesh <- traverse render (SpliceRenderable <$> x ^. _splices)
        liftEffect $ traverse_ (flip add comp) splices

        lfeet :: Array Object3D <- traverse render (LFootRenderable <$> x ^. _lfeet)
        liftEffect $ traverse_ (flip add comp) lfeet

        clamps :: Array Object3D <- traverse render (ClampRenderable <$> x ^. _clamps)
        liftEffect $ traverse_ (flip add comp) clamps

        stoppers :: Array Mesh <- traverse render (StopperRenderable <$> x ^. _stoppers)
        liftEffect $ traverse_ (flip add comp) stoppers

        pure comp

newtype RailRenderable = RailRenderable Rail
instance renderableRail :: Renderable e RailRenderable Mesh where
    render (RailRenderable rail) = liftEffect do
        mesh <- mkMesh railGeometry blackMaterial
        setName "Rail" mesh
        setScale (mkVec3 (meterVal $ rail ^. _length) 1.0 1.0) mesh
        setPosition (mkVec3 (meterVal $ rail ^. _x)
                            (meterVal $ rail ^. _y)
                            (meterVal $ rail ^. _z)) mesh
        pure mesh
        
railGeometry :: BoxGeometry
railGeometry = unsafePerformEffect $ mkBoxGeometry 1.0 h l
    where h = meterVal $ inch 1.0
          l = meterVal $ inch 2.5


newtype SpliceRenderable = SpliceRenderable Splice
instance renderableSplice :: Renderable e SpliceRenderable Mesh where
    render (SpliceRenderable s) = liftEffect do
        mesh <- mkMesh spliceGeometry blackMaterial
        setName "Splice" mesh
        setPosition (mkVec3 (meterVal $ s ^. _x)
                            (meterVal $ s ^. _y)
                            (meterVal $ s ^. _z)) mesh
        pure mesh

spliceGeometry :: BoxGeometry
spliceGeometry = unsafePerformEffect $ mkBoxGeometry 0.1778 0.02 0.06


newtype LFootRenderable = LFootRenderable LFoot
instance renderableLFoot :: Renderable e LFootRenderable Object3D where
    render (LFootRenderable l) = liftEffect do
        bot <- mkMesh lfootBotBox blackMaterial
        setName "bottom" bot
        setPosition (mkVec3 0.0 (-0.019) 0.0) bot

        side <- mkMesh lfootSideBox blackMaterial
        setName "side" side
        setPosition (mkVec3 0.0 0.0 0.035) side

        lf <- mkObject3D
        setName "LFoot" lf
        add bot lf
        add side lf
        setPosition (mkVec3 (meterVal $ l ^. _x)
                            (meterVal $ l ^. _y)
                            (meterVal $ l ^. _z)) lf
        pure lf

lfootBotBox :: BoxGeometry
lfootBotBox = unsafePerformEffect $ mkBoxGeometry 0.047752 0.048768 0.007874

lfootSideBox :: BoxGeometry
lfootSideBox = unsafePerformEffect $ mkBoxGeometry 0.047752 0.010668 0.0762


newtype ClampRenderable = ClampRenderable Clamp
instance renderableClamp :: Renderable e ClampRenderable Object3D where
    render (ClampRenderable c) = liftEffect do
        m <- buildClamp
        setPosition (mkVec3 (meterVal $ c ^. _x)
                            (meterVal $ c ^. _y)
                            (meterVal $ c ^. _z)) m
        pure m

newtype StopperRenderable = StopperRenderable Stopper
instance renderableStopper :: Renderable e StopperRenderable Mesh where
    render (StopperRenderable s) = liftEffect do
        m <- mkMesh stopperCy blackMaterial
        setName "Stopper" m
        setRotation (mkEuler (pi / 2.0) 0.0 0.0) m
        setPosition (mkVec3 (meterVal $ s ^. _x)
                            (meterVal $ s ^. _y)
                            (meterVal $ s ^. _z)) m
        pure m

stopperCy :: CylinderGeometry
stopperCy = unsafePerformEffect $ mkCylinderGeometry 0.02 0.036
