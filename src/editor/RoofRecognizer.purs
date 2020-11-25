module Editor.PolygonAdder where

import Prelude

import Custom.Mesh (TappableMesh, mkTappableMesh)
import Data.Compactable (compact)
import Data.Lens (Lens', view, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Editor.Common.Lenses (_disposable, _mesh, _tapped)
import Editor.Disposable (class Disposable)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, sampleDyn, subscribeDyn)
import FRP.Event (Event)
import FRP.Event.Extra (multicast, performEvent)
import Model.Roof.RoofPlate (RoofPlate, newRoofPlate)
import Three.Core.Geometry (CircleGeometry, mkCircleGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial)
import Three.Core.Mesh (Mesh)
import Three.Core.Object3D (class IsObject3D, Object3D, hasParent, localToWorld, lookAt, parent, setName, setPosition, setVisible, toObject3D)
import Three.Math.Vector (Vector3, addScaled, (<+>))

-- | PolygonAdder will be able to let user add new roof
newtype PolygonAdder = PolygonAdder {
    marker     :: Mesh,
    addedPoint :: Event CandidatePoint,
    disposable :: Effect Unit
}

derive instance newtypePolygonAdder :: Newtype PolygonAdder _
instance isObject3DPolygonAdder :: IsObject3D PolygonAdder where
    toObject3D = toObject3D <<< view _marker
instance disposePolygonAdder :: Disposable PolygonAdder where
    dispose r = r ^. _disposable

_marker :: Lens' PolygonAdder Mesh
_marker = _Newtype <<< prop (SProxy :: SProxy "marker")

_addedPoint :: Lens' PolygonAdder (Event RoofPlate)
_addedPoint = _Newtype <<< prop (SProxy :: SProxy "addedPoint")

-- | Candidate point that will allow user to show the adder marker
type CandidatePoint = {
    position   :: Vector3,
    faceNormal :: Vector3
}

adderMarkerMat :: MeshBasicMaterial
adderMarkerMat = unsafePerformEffect (mkMeshBasicMaterial 0x2222ff)

adderMarkerGeo :: CircleGeometry
adderMarkerGeo = unsafePerformEffect (mkCircleGeometry 1.0 32)

createAdderMarker :: Effect TappableMesh
createAdderMarker = do
    marker <- mkTappableMesh adderMarkerGeo adderMarkerMat
    setName "add-roof-marker" marker
    pure marker


showMarker :: TappableMesh -> Maybe CandidatePoint -> Effect Unit
showMarker marker Nothing = setVisible false marker
showMarker marker (Just p) | not (hasParent marker) = setVisible false marker
                           | otherwise = do
                                 setVisible true marker
                                 -- get the local position of the candidate point
                                 -- and move it along the normal vector a bit.
                                 -- then used as the new position of the marker
                                 let np = addScaled p.position p.faceNormal 0.03
                                 setPosition np marker

                                 -- set the target direction of the marker
                                 let target = p.position <+> p.faceNormal
                                 targetW <- localToWorld target (parent marker :: Object3D)
                                 lookAt targetW marker


-- | create a roof recognizer
createPolygonAdder :: Dynamic (Maybe CandidatePoint) -> Dynamic Boolean -> Effect PolygonAdder
createPolygonAdder point canShow = do
    marker <- createAdderMarker

    -- hide the marker by default
    setVisible false marker

    let 
        -- update candidate point with canShow status
        pointCanShow true p  = p
        pointCanShow false _ = Nothing

    d <- subscribeDyn (pointCanShow <$> canShow <*> point) (showMarker marker)

    let roof = compact $ performEvent $ sampleDyn point (flip const <$> marker ^. _tapped)
    pure $ PolygonAdder {
        marker     : marker ^. _mesh,
        addedPoint : multicast roof,
        disposable : d
    }