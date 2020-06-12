module Rendering.Racking.FXRendering where

import Prelude hiding (add)

import Data.Foldable (traverse_)
import Data.Lens ((^.))
import Data.Meter (meterVal)
import Data.Traversable (traverse)
import Editor.Common.Lenses (_bridges, _flashes, _leftEndCaps, _length, _mounts, _rightEndCaps, _skirts, _x, _y, _z)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Math (atan, pi, sqrt)
import Model.Racking.FX.Bridge (Bridge)
import Model.Racking.FX.EndCap (EndCap)
import Model.Racking.FX.FXRackingComponent (FXRackingComponent)
import Model.Racking.FX.Mount (Mount, _clampX, mountRadius)
import Model.Racking.FX.Skirt (Skirt)
import Rendering.Racking.Common (FlashRenderable(..), buildClamp)
import Rendering.Renderable (class Renderable, render)
import Renderring.MaterialLoader (blackMaterial)
import Three.Core.Geometry (BoxGeometry, CylinderGeometry, mkBoxGeometry, mkCylinderGeometry)
import Three.Core.Mesh (Mesh, mkMesh)
import Three.Core.Object3D (Object3D, add, mkObject3D, setName, setPosition, setRotation, setScale)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (mkVec3)


newtype FXRackingCOmponentRenderable = FXRackingCOmponentRenderable FXRackingComponent
instance renderableFXRackingComponent :: Renderable FXRackingCOmponentRenderable Object3D where
    render (FXRackingCOmponentRenderable f) = do
        comp <- mkObject3D
        setName "FXRackingComponent" comp

        flashes :: Array Mesh <- traverse render (FlashRenderable <$> f ^. _flashes)
        traverse_ (flip add comp) flashes

        mounts :: Array Object3D <- traverse render (MountRenderable <$> f ^. _mounts)
        traverse_ (flip add comp) mounts

        bridges :: Array Object3D <- traverse render (BridgeRenderable <$> f ^. _bridges)
        traverse_ (flip add comp) bridges

        skirts :: Array Mesh <- traverse render (SkirtRenderable <$> f ^. _skirts)
        traverse_ (flip add comp) skirts

        endCaps :: Array Mesh <- traverse render (EndCapRenderable <$> (f ^. _leftEndCaps <> f ^. _rightEndCaps))
        traverse_ (flip add comp) endCaps

        pure comp


mountBotBox :: BoxGeometry
mountBotBox = unsafePerformEffect $ mkBoxGeometry 0.0508 0.127 0.05

mountEndCy :: CylinderGeometry
mountEndCy = unsafePerformEffect $ mkCylinderGeometry 0.0254 0.05

mountHeadCy :: CylinderGeometry
mountHeadCy = unsafePerformEffect $ mkCylinderGeometry 0.0127 0.046

buildMount :: Effect Object3D
buildMount = do
    bot <- mkMesh mountBotBox blackMaterial
    setName "bottom" bot

    botEnd1 <- mkMesh mountEndCy blackMaterial
    setName "bottom-end-1" botEnd1
    setRotation (mkEuler (pi / 2.0) 0.0 0.0) botEnd1
    setPosition (mkVec3 0.0 0.06 0.0) botEnd1

    botEnd2 <- mkMesh mountEndCy blackMaterial
    setName "bottom-end-2" botEnd2
    setRotation (mkEuler (pi / 2.0) 0.0 0.0) botEnd2
    setPosition (mkVec3 0.0 (-0.06) 0.0) botEnd2

    head <- mkMesh mountHeadCy blackMaterial
    setName "head" head
    setPosition (mkVec3 0.0 (-0.058) 0.046) head

    m <- mkObject3D
    setName "Mount" m
    add bot m
    add botEnd1 m
    add botEnd2 m
    add head m

    pure m


newtype MountRenderable = MountRenderable Mount
instance renderableMount :: Renderable MountRenderable Object3D where
    render (MountRenderable m) = do
        let bx = meterVal $ m ^. _clampX - m ^. _x

        clamp <- buildClamp
        setName "Clamp-part" clamp
        setPosition (mkVec3 bx (-0.015) 0.0433) clamp
        
        mount <- buildMount
        setName "Mount-part" mount
        -- calculate the center position and rotation of the mount
        -- "a" denotes the end of the mount on flash, (ax is always 0)
        -- "b" denotes the end of the mount with clamp (by is always 0)
        -- "c" denotes center of the mount
        let r = meterVal mountRadius
            aY = sqrt $ r * r - bx * bx
            cX = bx / 2.0
            cY = aY / 2.0
        
        setRotation (mkEuler 0.0 0.0 (atan (-bx / aY))) mount
        setPosition (mkVec3 cX (cY - r / 2.0) 0.0) mount

        w <- mkObject3D
        setName "Mount" w
        add clamp w
        add mount w
        setPosition (mkVec3 (meterVal $ m ^. _x)
                            (meterVal $ m ^. _y)
                            (meterVal $ m ^. _z)) w

        pure w


newtype BridgeRenderable = BridgeRenderable Bridge
instance renderableBridge :: Renderable BridgeRenderable Object3D where
    render (BridgeRenderable b) = do
        top <- mkMesh bridgeTopBox blackMaterial
        setName "top" top
        setPosition (mkVec3 0.0 0.0 0.02) top

        mid <- mkMesh bridgeMidBox blackMaterial
        setName "middle" mid

        bot <- mkMesh bridgeBotBox blackMaterial
        setName "bottom" bot
        setPosition (mkVec3 0.0 0.0 (-0.02)) bot

        w <- mkObject3D
        setName "Bridge" w
        add top w
        add mid w 
        add bot w
        setPosition (mkVec3 (meterVal $ b ^. _x)
                            (meterVal $ b ^. _y)
                            (meterVal $ b ^. _z)) w

        pure w

bridgeTopBox :: BoxGeometry
bridgeTopBox = unsafePerformEffect $ mkBoxGeometry 0.2032 0.05 0.003

bridgeMidBox :: BoxGeometry
bridgeMidBox = unsafePerformEffect $ mkBoxGeometry 0.2032 0.02 0.04

bridgeBotBox :: BoxGeometry
bridgeBotBox = unsafePerformEffect $ mkBoxGeometry 0.2032 0.1 0.003


newtype SkirtRenderable = SkirtRenderable Skirt
instance renderableSkirt :: Renderable SkirtRenderable Mesh where
    render (SkirtRenderable s) = do
        m <- mkMesh skirtBox blackMaterial
        setName "Skirt" m
        setScale (mkVec3 (meterVal $ s ^. _length) 1.0 1.0) m
        setPosition (mkVec3 (meterVal $ s ^. _x)
                            (meterVal $ s ^. _y)
                            (meterVal $ s ^. _z)) m
        
        pure m

skirtBox :: BoxGeometry
skirtBox = unsafePerformEffect $ mkBoxGeometry 1.0 0.03 0.04

newtype EndCapRenderable = EndCapRenderable EndCap
instance renderableEndCap :: Renderable EndCapRenderable Mesh where
    render (EndCapRenderable e) = do
        m <- mkMesh endCapBox blackMaterial
        setName "EndCap" m
        setPosition (mkVec3 (meterVal $ e ^. _x)
                            (meterVal $ e ^. _y)
                            (meterVal $ e ^. _z)) m
        pure m

endCapBox :: BoxGeometry
endCapBox = unsafePerformEffect $ mkBoxGeometry 0.001 0.03 0.04
