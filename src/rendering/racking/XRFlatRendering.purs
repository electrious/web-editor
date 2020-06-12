module Rendering.Racking.XRFlatRendering where

import Prelude hiding (add)

import Data.Lens ((^.))
import Data.Meter (inch, meterVal)
import Editor.Common.Lenses (_height, _length, _x, _y, _z)
import Effect.Unsafe (unsafePerformEffect)
import Math (pi)
import Math.Angle (radianVal)
import Model.Racking.XR10.Clamp (Clamp)
import Model.Racking.XR10.Rail (Rail)
import Model.Racking.XR10.Splice (Splice)
import Model.Racking.XR10.Stopper (Stopper)
import Model.Racking.XRFlat.QBaseMount (QBaseMount)
import Model.Racking.XRFlat.SupportRail (SupportRail)
import Model.Racking.XRFlat.TiltLeg (TiltLeg)
import Rendering.Racking.Common (buildClamp)
import Rendering.Racking.XRRendering (railGeometry, spliceGeometry, stopperCy)
import Rendering.Renderable (class RenderableWithSlope)
import Renderring.MaterialLoader (blackMaterial)
import Three.Core.Geometry (BoxGeometry, CylinderGeometry, mkBoxGeometry, mkCylinderGeometry)
import Three.Core.Mesh (Mesh, mkMesh)
import Three.Core.Object3D (Object3D, add, mkObject3D, setName, setPosition, setRotation, setScale)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (mkVec3)


newtype RailRenderableWithSlope = RailRenderableWithSlope Rail
instance renderableWithSlopeRail :: RenderableWithSlope RailRenderableWithSlope Mesh where
    renderWithSlope (RailRenderableWithSlope r) slope = do
        rail <- mkMesh railGeometry blackMaterial
        setName "Rail" rail
        setScale (mkVec3 (meterVal $ r ^. _length) 1.0 1.0) rail
        setPosition (mkVec3 (meterVal $ r ^. _x)
                            (meterVal $ r ^. _y)
                            (meterVal $ r ^. _z)) rail
        setRotation (mkEuler (radianVal slope) 0.0 0.0) rail

        pure rail


newtype SpliceRenderableWithSlope = SpliceRenderableWithSlope Splice
instance renderableWithSlopeSplice :: RenderableWithSlope SpliceRenderableWithSlope Mesh where
    renderWithSlope (SpliceRenderableWithSlope s) slope = do
        splice <- mkMesh spliceGeometry blackMaterial
        setName "Splice" splice
        setPosition (mkVec3 (meterVal $ s ^. _x)
                            (meterVal $ s ^. _y)
                            (meterVal $ s ^. _z)) splice
        setRotation (mkEuler (radianVal slope) 0.0 0.0) splice

        pure splice


newtype ClampRenderableWithSlope = ClampRenderableWithSlope Clamp
instance renderableWithSlopeClamp :: RenderableWithSlope ClampRenderableWithSlope Object3D where
    renderWithSlope (ClampRenderableWithSlope c) slope = do
        clamp <- buildClamp
        setName "Clamp" clamp
        setPosition (mkVec3 (meterVal $ c ^. _x)
                            (meterVal $ c ^. _y)
                            (meterVal $ c ^. _z)) clamp
        setRotation (mkEuler (radianVal slope) 0.0 0.0) clamp
        pure clamp


newtype StopperRenderableWithSlope = StopperRenderableWithSlope Stopper
instance renderableWithSlopeStopper :: RenderableWithSlope StopperRenderableWithSlope Mesh where
    renderWithSlope (StopperRenderableWithSlope s) slope = do 
        stopper <- mkMesh stopperCy blackMaterial
        setName "Stopper" stopper
        setRotation (mkEuler (pi / 2.0 + radianVal slope) 0.0 0.0) stopper
        setPosition (mkVec3 (meterVal $ s ^. _x)
                            (meterVal $ s ^. _y)
                            (meterVal $ s ^. _z)) stopper
        pure stopper


newtype SupportRailRenderableWithSlope = SupportRailRenderableWithSlope SupportRail
instance renderableWithSlopeSupportRail :: RenderableWithSlope SupportRailRenderableWithSlope Mesh where
    renderWithSlope (SupportRailRenderableWithSlope s) slope = do
        r <- mkMesh supportRailGeo blackMaterial
        setName "SupportRail" r
        setScale (mkVec3 1.0 (meterVal $ s ^. _length) 1.0) r
        setPosition (mkVec3 (meterVal $ s ^. _x)
                            (meterVal $ s ^. _y)
                            (meterVal $ s ^. _z)) r
        pure r


supportRailGeo :: BoxGeometry
supportRailGeo = unsafePerformEffect $ mkBoxGeometry l 1.0 l
    where l = meterVal $ inch 1.0


newtype QBaseMountRenderable = QBaseMountRenderable QBaseMount
instance renderableWithSlopeQBaseMount :: RenderableWithSlope QBaseMountRenderable Object3D where
    renderWithSlope (QBaseMountRenderable q) slope = do
        base <- mkMesh qbMountBaseCy blackMaterial
        setName "base" base
        setPosition (mkVec3 0.0 (meterVal $ inch 0.25) 0.0) base

        stick <- mkMesh qbMountStickCy blackMaterial
        setName "stick" stick
        setPosition (mkVec3 0.0 (meterVal (q ^. _height) / 2.0) 0.0) stick
        setScale (mkVec3 1.0 (meterVal $ q ^. _height) 1.0) stick

        m <- mkObject3D
        setName "QBaseMount" m
        add base m
        add stick m
        setRotation (mkEuler (pi / 2.0) 0.0 0.0) m
        setPosition (mkVec3 (meterVal $ q ^. _x)
                            (meterVal $ q ^. _y)
                            (meterVal $ q ^. _z)) m
        
        pure m

qbMountBaseCy :: CylinderGeometry
qbMountBaseCy = unsafePerformEffect $ mkCylinderGeometry (meterVal $ inch 3.0) (meterVal $ inch 0.5)

qbMountStickCy :: CylinderGeometry
qbMountStickCy = unsafePerformEffect $ mkCylinderGeometry (meterVal $ inch 0.8) 1.0


newtype TiltLegRenderable = TiltLegRenderable TiltLeg
instance renderableWithSlopeTiltLeg :: RenderableWithSlope TiltLegRenderable Mesh where
    renderWithSlope (TiltLegRenderable t) slope = do
        m <- mkMesh tiltLegBox blackMaterial
        setName "TiltLeg" m
        setPosition (mkVec3 (meterVal $ t ^. _x)
                            (meterVal $ t ^. _y)
                            (meterVal $ t ^. _z)) m
        setScale (mkVec3 1.0 1.0 (meterVal $ t ^. _length)) m
        pure m

tiltLegBox :: BoxGeometry
tiltLegBox = unsafePerformEffect $ mkBoxGeometry l l 1.0
    where l = meterVal $ inch 0.8
