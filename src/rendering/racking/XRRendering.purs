module Rendering.Racking.XRRendering where

import Prelude

import Data.Lens ((^.))
import Data.Meter (inch, meterVal)
import Editor.Common.Lenses (_length, _x, _y, _z)
import Effect.Unsafe (unsafePerformEffect)
import Math (pi)
import Model.Racking.XR10.Clamp (Clamp)
import Model.Racking.XR10.LFoot (LFoot)
import Model.Racking.XR10.Rail (Rail)
import Model.Racking.XR10.Splice (Splice)
import Model.Racking.XR10.Stopper (Stopper)
import Rendering.Renderable (class Renderable)
import Renderring.MaterialLoader (blackMaterial)
import Three.Core.Geometry (BoxGeometry, CylinderGeometry, mkBoxGeometry, mkCylinderGeometry)
import Three.Core.Mesh (Mesh, mkMesh)
import Three.Core.Object3D (Object3D, add, mkObject3D, setName, setPosition, setRotation, setScale)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (mkVec3)

newtype RailRenderable = RailRenderable Rail
instance renderableRail :: Renderable RailRenderable Mesh where
    render (RailRenderable rail) = do
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
instance renderableSplice :: Renderable SpliceRenderable Mesh where
    render (SpliceRenderable s) = do
        mesh <- mkMesh spliceGeometry blackMaterial
        setName "Splice" mesh
        setPosition (mkVec3 (meterVal $ s ^. _x)
                            (meterVal $ s ^. _y)
                            (meterVal $ s ^. _z)) mesh
        pure mesh

spliceGeometry :: BoxGeometry
spliceGeometry = unsafePerformEffect $ mkBoxGeometry 0.1778 0.02 0.06


newtype LFootRenderable = LFootRenderable LFoot
instance renderableLFoot :: Renderable LFootRenderable Object3D where
    render (LFootRenderable l) = do
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
instance renderableClamp :: Renderable ClampRenderable Object3D where
    render (ClampRenderable c) = do
        body <- mkMesh clampBodyCy blackMaterial
        setName "body" body
        setRotation (mkEuler (pi / 2.0) 0.0 0.0) body
        setPosition (mkVec3 0.0 0.0 0.02) body

        head <- mkMesh clampHeadCy blackMaterial
        setName "head" head
        setRotation (mkEuler (pi / 2.0) 0.0 0.0) head
        setPosition (mkVec3 0.0 0.0 0.043) head

        m <- mkObject3D
        setName "Clamp" m
        add body m
        add head m
        setPosition (mkVec3 (meterVal $ c ^. _x)
                            (meterVal $ c ^. _y)
                            (meterVal $ c ^. _z)) m
        pure m

clampBodyCy :: CylinderGeometry
clampBodyCy = unsafePerformEffect $ mkCylinderGeometry 0.005 0.06

clampHeadCy :: CylinderGeometry
clampHeadCy = unsafePerformEffect $ mkCylinderGeometry 0.015 0.006


newtype StopperRenderable = StopperRenderable Stopper
instance renderableStopper :: Renderable StopperRenderable Mesh where
    render (StopperRenderable s) = do
        m <- mkMesh stopperCy blackMaterial
        setName "Stopper" m
        setRotation (mkEuler (pi / 2.0) 0.0 0.0) m
        setPosition (mkVec3 (meterVal $ s ^. _x)
                            (meterVal $ s ^. _y)
                            (meterVal $ s ^. _z)) m
        pure m

stopperCy :: CylinderGeometry
stopperCy = unsafePerformEffect $ mkCylinderGeometry 0.02 0.036
