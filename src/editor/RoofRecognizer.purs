module Editor.RoofRecognizer where

import Prelude

import Algorithm.RoofCheck (couldBeRoof)
import Custom.Mesh (TappableMesh, mkTappableMesh)
import Data.Maybe (Maybe(..))
import Editor.SceneEvent (SceneMouseMoveEvent)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (Event, gate, keepLatest, sampleOn)
import Models.RoofPlate (RoofPlate, newRoofPlate)
import Three.Core.Face3 (normal)
import Three.Core.Geometry (Geometry, mkCircleGeometry)
import Three.Core.Material (Material, mkMeshBasicMaterial)
import Three.Core.Mesh (Mesh)
import Three.Core.Object3D (Object3D, hasParent, localToWorld, lookAt, parent, setName, setPosition, setVisible, worldToLocal)
import Three.Math.Vector (Vector3, addScaled, mkVec3, (<+>))
import Unsafe.Coerce (unsafeCoerce)
import Util (multicast, performEvent)

-- | RoofRecognizer will be able to let user add new roof
type RoofRecognizer a = {
    marker       :: Mesh a,
    addedNewRoof :: Event RoofPlate
}

-- | Candidate point that will allow user to show the adder marker
type CandidatePoint = {
    position   :: Vector3,
    faceNormal :: Vector3
}

adderMarkerMat :: forall mat. Material mat
adderMarkerMat = unsafeCoerce $ unsafePerformEffect (mkMeshBasicMaterial 0x2222ff)

adderMarkerGeo :: forall geo. Geometry geo
adderMarkerGeo = unsafeCoerce $ unsafePerformEffect (mkCircleGeometry 1.0 32)


type RoofAdder a = {
    marker   :: TappableMesh a,
    position :: Vector3,
    normal   :: Vector3
}

createAdderMarker :: forall a. Effect (RoofAdder a)
createAdderMarker = do
    marker <- mkTappableMesh adderMarkerGeo adderMarkerMat
    setName "add-roof-marker" marker.mesh

    pure {
        marker: marker,
        position: mkVec3 0.0 0.0 0.0,
        normal: mkVec3 0.0 1.0 0.0
    }


showMarker :: forall a. RoofAdder a -> Maybe CandidatePoint -> Effect (RoofAdder a)
showMarker adder Nothing = setVisible false adder.marker.mesh *> pure adder
showMarker adder (Just p) | not (hasParent adder.marker.mesh) = setVisible false adder.marker.mesh *> pure adder
                          | otherwise = do
                              setVisible true adder.marker.mesh
                              -- get the local position of the candidate point
                              -- and move it along the normal vector a bit.
                              -- then used as the new position of the marker
                              let np = addScaled p.position p.faceNormal 0.03
                              setPosition np adder.marker.mesh

                              -- set the target direction of the marker
                              let target = p.position <+> p.faceNormal
                              targetW <- localToWorld target (parent adder.marker.mesh)
                              lookAt targetW adder.marker.mesh
                              pure $ adder { position = np, normal = p.faceNormal }


-- | create a roof recognizer
createRoofRecognizer :: forall a b. Object3D a
                               -> Event (Array RoofPlate)
                               -> Event SceneMouseMoveEvent
                               -> Event Boolean
                               -> Effect (RoofRecognizer b)
createRoofRecognizer houseWrapper roofs mouseMove canShow = do
    adder <- createAdderMarker
    let marker = adder.marker

    -- hide the marker by default
    setVisible false marker.mesh

    let f evt rs = do
            isRoof <- couldBeRoof houseWrapper rs evt
            if isRoof
            then do
                np <- worldToLocal evt.point houseWrapper
                pure $ Just { position: np, faceNormal: normal evt.face }
            else pure Nothing

        point = performEvent $ sampleOn roofs (f <$> mouseMove)
    
        mkRoof a = newRoofPlate a.position a.normal
        adderEvt = performEvent (showMarker adder <$> gate canShow point)
        adderTapped a = const a <$> a.marker.tapped
        roof = mkRoof <$> keepLatest (adderTapped <$> adderEvt)
    pure {
        marker: marker.mesh,
        addedNewRoof: multicast $ performEvent roof
    }