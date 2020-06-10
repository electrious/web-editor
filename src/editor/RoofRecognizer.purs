module Editor.RoofRecognizer where

import Prelude

import Algorithm.RoofCheck (couldBeRoof)
import Custom.Mesh (TappableMesh, mkTappableMesh)
import Data.Compactable (compact)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Editor.Common.Lenses (_disposable, _face, _mesh, _point, _tapped)
import Editor.Disposable (class Disposable)
import Editor.SceneEvent (SceneMouseMoveEvent)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, gateDyn, sampleDyn, step, subscribeDyn)
import FRP.Event (Event, sampleOn)
import FRP.Event.Extra (multicast, performEvent)
import Model.Roof.RoofPlate (RoofPlate, newRoofPlate)
import Three.Core.Face3 (normal)
import Three.Core.Geometry (CircleGeometry, mkCircleGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial)
import Three.Core.Mesh (Mesh)
import Three.Core.Object3D (class IsObject3D, Object3D, hasParent, localToWorld, lookAt, parent, setName, setPosition, setVisible, worldToLocal)
import Three.Math.Vector (Vector3, addScaled, (<+>))
import Unsafe.Coerce (unsafeCoerce)

-- | RoofRecognizer will be able to let user add new roof
newtype RoofRecognizer = RoofRecognizer {
    marker       :: Mesh,
    addedNewRoof :: Event RoofPlate,
    disposable   :: Effect Unit
}

derive instance newtypeRoofRecognizer :: Newtype RoofRecognizer _
instance disposeRoofRecognizer :: Disposable RoofRecognizer where
    dispose r = r ^. _disposable

_marker :: Lens' RoofRecognizer Mesh
_marker = _Newtype <<< prop (SProxy :: SProxy "marker")

_addedNewRoof :: Lens' RoofRecognizer (Event RoofPlate)
_addedNewRoof = _Newtype <<< prop (SProxy :: SProxy "addedNewRoof")

-- | Candidate point that will allow user to show the adder marker
type CandidatePoint = {
    position   :: Vector3,
    faceNormal :: Vector3
}

adderMarkerMat :: MeshBasicMaterial
adderMarkerMat = unsafeCoerce $ unsafePerformEffect (mkMeshBasicMaterial 0x2222ff)

adderMarkerGeo :: CircleGeometry
adderMarkerGeo = unsafeCoerce $ unsafePerformEffect (mkCircleGeometry 1.0 32)

createAdderMarker :: Effect TappableMesh
createAdderMarker = do
    marker <- mkTappableMesh adderMarkerGeo adderMarkerMat
    setName "add-roof-marker" $ marker ^. _mesh
    pure marker


showMarker :: TappableMesh -> Maybe CandidatePoint -> Effect Unit
showMarker marker Nothing = setVisible false $ marker ^. _mesh
showMarker marker (Just p) | not (hasParent $ marker ^. _mesh) = setVisible false $ marker ^. _mesh
                           | otherwise = do
                                 setVisible true $ marker ^. _mesh
                                 -- get the local position of the candidate point
                                 -- and move it along the normal vector a bit.
                                 -- then used as the new position of the marker
                                 let np = addScaled p.position p.faceNormal 0.03
                                 setPosition np $ marker ^. _mesh

                                 -- set the target direction of the marker
                                 let target = p.position <+> p.faceNormal
                                 targetW <- localToWorld target (parent (marker ^. _mesh) :: Object3D)
                                 lookAt targetW $ marker ^. _mesh


-- | create a roof recognizer
createRoofRecognizer :: forall a. IsObject3D a =>
                               a
                               -> Event (Array RoofPlate)
                               -> Event SceneMouseMoveEvent
                               -> Dynamic Boolean
                               -> Effect RoofRecognizer
createRoofRecognizer houseWrapper roofs mouseMove canShow = do
    marker <- createAdderMarker

    -- hide the marker by default
    setVisible false $ marker ^. _mesh

    let getCandidatePoint evt rs = do
            isRoof <- couldBeRoof houseWrapper rs evt
            if isRoof
            then do
                np <- worldToLocal (evt ^. _point) houseWrapper
                pure $ Just { position: np, faceNormal: normal (evt ^. _face) }
            else pure Nothing

        point = step Nothing $ performEvent $ sampleOn roofs (getCandidatePoint <$> gateDyn canShow mouseMove)
    
        mkRoof _ p = newRoofPlate p.position p.faceNormal

        -- update candidate point with canShow status
        pointCanShow true p = p
        pointCanShow false _ = Nothing

    d <- subscribeDyn (pointCanShow <$> canShow <*> point) (showMarker marker)

    let roof = compact $ performEvent $ sampleDyn point (traverse <<< mkRoof <$> marker ^. _tapped)
    pure $ RoofRecognizer {
        marker       : marker ^. _mesh,
        addedNewRoof : multicast roof,
        disposable   : d
    }