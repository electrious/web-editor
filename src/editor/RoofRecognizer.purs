module Editor.RoofRecognizer where

import Prelude

import Algorithm.RoofCheck (couldBeRoof)
import Control.Apply (lift2)
import Custom.Mesh (TappableMesh, mkTappableMesh)
import Data.Compactable (compact)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Editor.SceneEvent (SceneMouseMoveEvent, _face, _mousePoint)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (Event, gate, sampleOn, subscribe)
import FRP.Event.Extra (multicast, performEvent)
import Models.RoofPlate (RoofPlate, newRoofPlate)
import Three.Core.Face3 (normal)
import Three.Core.Geometry (Geometry, mkCircleGeometry)
import Three.Core.Material (Material, mkMeshBasicMaterial)
import Three.Core.Mesh (Mesh)
import Three.Core.Object3D (Object3D, hasParent, localToWorld, lookAt, parent, setName, setPosition, setVisible, worldToLocal)
import Three.Math.Vector (Vector3, addScaled, (<+>))
import Unsafe.Coerce (unsafeCoerce)

-- | RoofRecognizer will be able to let user add new roof
type RoofRecognizer a = {
    marker       :: Mesh a,
    addedNewRoof :: Event RoofPlate,
    disposable   :: Effect Unit
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

createAdderMarker :: forall a. Effect (TappableMesh a)
createAdderMarker = do
    marker <- mkTappableMesh adderMarkerGeo adderMarkerMat
    setName "add-roof-marker" marker.mesh
    pure marker


showMarker :: forall a. TappableMesh a -> Maybe CandidatePoint -> Effect Unit
showMarker marker Nothing = setVisible false marker.mesh
showMarker marker (Just p) | not (hasParent marker.mesh) = setVisible false marker.mesh
                           | otherwise = do
                                 setVisible true marker.mesh
                                 -- get the local position of the candidate point
                                 -- and move it along the normal vector a bit.
                                 -- then used as the new position of the marker
                                 let np = addScaled p.position p.faceNormal 0.03
                                 setPosition np marker.mesh

                                 -- set the target direction of the marker
                                 let target = p.position <+> p.faceNormal
                                 targetW <- localToWorld target (parent marker.mesh)
                                 lookAt targetW marker.mesh


-- | create a roof recognizer
createRoofRecognizer :: forall a b. Object3D a
                               -> Event (Array RoofPlate)
                               -> Event SceneMouseMoveEvent
                               -> Event Boolean
                               -> Effect (RoofRecognizer b)
createRoofRecognizer houseWrapper roofs mouseMove canShow = do
    marker <- createAdderMarker

    -- hide the marker by default
    setVisible false marker.mesh

    let getCandidatePoint evt rs = do
            isRoof <- couldBeRoof houseWrapper rs evt
            if isRoof
            then do
                np <- worldToLocal (evt ^. _mousePoint) houseWrapper
                pure $ Just { position: np, faceNormal: normal (evt ^. _face) }
            else pure Nothing

        point = performEvent $ sampleOn roofs (getCandidatePoint <$> gate canShow mouseMove)
    
        mkRoof _ p = newRoofPlate p.position p.faceNormal

        -- update candidate point with canShow status
        pointCanShow true p = p
        pointCanShow false _ = Nothing

    d <- subscribe (lift2 pointCanShow canShow point) (showMarker marker)

    let roof = compact $ performEvent $ sampleOn point (traverse <<< mkRoof <$> marker.tapped)
    pure {
        marker: marker.mesh,
        addedNewRoof: multicast roof,
        disposable: d
    }