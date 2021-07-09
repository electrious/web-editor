module Editor.ObjectAdder where

import Prelude

import Data.Compactable (compact)
import Data.Default (def)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype)
import Type.Proxy (Proxy(..))
import Data.Traversable (traverse)
import Editor.Common.Lenses (_name, _position, _rotation, _tapped)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, performDynamic, sampleDyn_)
import FRP.Event (Event)
import Math (pi)
import Rendering.Node (Node, _raycastable, _target, _visible, getParent, mesh, node, tapMesh)
import Three.Core.Geometry (BoxGeometry, CircleGeometry, mkBoxGeometry, mkCircleGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial)
import Three.Core.Object3D (localToWorld)
import Three.Math.Euler (Euler, mkEuler)
import Three.Math.Vector (class Vector, Vector3, addScaled, getVector, mkVec3, (<+>))


data AdderType = DefaultAdder
               | CrossAdder

-- | Candidate point that will allow user to show the adder marker
newtype CandidatePoint v = CandidatePoint {
    position   :: v,
    faceNormal :: Vector3
}

derive instance newtypeCandidatePoint :: Newtype (CandidatePoint v) _

_faceNormal :: forall t a r. Newtype t { faceNormal :: a | r } => Lens' t a
_faceNormal = _Newtype <<< prop (Proxy :: Proxy "faceNormal")

mkCandidatePoint :: forall v. v -> Vector3 -> CandidatePoint v
mkCandidatePoint p n = CandidatePoint { position : p, faceNormal : n }

adderMarkerMat :: MeshBasicMaterial
adderMarkerMat = unsafePerformEffect (mkMeshBasicMaterial 0x2222ff)

adderMarkerGeo :: CircleGeometry
adderMarkerGeo = unsafePerformEffect (mkCircleGeometry 1.0 32)

-- the default blue circle marker
blueMarker :: forall e. Dynamic Boolean -> Node e Unit
blueMarker visDyn = void $ mesh (def # _name    .~ "blue-marker"
                                     # _visible .~ visDyn
                                ) adderMarkerGeo adderMarkerMat


whiteGeo :: BoxGeometry
whiteGeo = unsafePerformEffect $ mkBoxGeometry 2.0 0.3 0.0001

whiteMat :: MeshBasicMaterial
whiteMat = unsafePerformEffect $ mkMeshBasicMaterial 0xffffff


blackGeo :: BoxGeometry
blackGeo = unsafePerformEffect $ mkBoxGeometry 1.8 0.1 0.0001

blackMat :: MeshBasicMaterial
blackMat = unsafePerformEffect $ mkMeshBasicMaterial 0x000000


-- a single line of the cross marker
blackOnWhiteLine :: forall e. Euler -> Node e Unit
blackOnWhiteLine rot = node (def # _name     .~ "cross-line"
                                 # _rotation .~ pure rot
                            ) do
    void $ mesh (def # _name .~ "white-line") whiteGeo whiteMat
    void $ mesh (def # _name .~ "black-line"
                     # _position .~ pure (mkVec3 0.0 0.0 0.001)
                ) blackGeo blackMat


crossMarker :: forall e. Dynamic Boolean -> Node e Unit
crossMarker visDyn = node (def # _name    .~ "cross-marker"
                               # _visible .~ visDyn) do
    blackOnWhiteLine def
    blackOnWhiteLine (mkEuler 0.0 0.0 (pi / 2.0))


createAdderMarker :: forall e v. Vector v => Boolean -> Dynamic (Maybe (CandidatePoint v)) -> (Dynamic Boolean -> Node e Unit) -> Node e (Event (CandidatePoint v))
createAdderMarker updTarget pDyn marker = do
    parent <- getParent

    let posV p = getVector $ p ^. _position
        -- get the local position of the candidate point
        -- and move it along the normal vector a bit.
        -- then used as the new position of the marker
        calcPos Nothing  = def
        calcPos (Just p) = addScaled (posV p) (p ^. _faceNormal) 0.1

        calcTarget p = localToWorld (posV p <+> p ^. _faceNormal) parent

        posDyn    = calcPos <$> pDyn
        targetDyn = performDynamic $ traverse calcTarget <$> pDyn

        visDyn = isJust <$> pDyn

        prop = def # _name     .~ "adder-marker"
                   # _position .~ posDyn
        
        nProp = if updTarget
                then prop # _target   .~ targetDyn
                else prop 
    m <- node nProp do
        -- the visible marker
        marker visDyn
        tapMesh (def # _visible .~ pure false
                     # _raycastable .~ visDyn
                ) adderMarkerGeo adderMarkerMat
    
    pure $ compact $ sampleDyn_ pDyn $ m ^. _tapped

-- | create a object adder
createObjectAdder :: forall e v. Vector v => AdderType -> Dynamic (Maybe (CandidatePoint v)) -> Dynamic Boolean -> Node e (Event (CandidatePoint v))
createObjectAdder t point canShow = createAdderMarker (updTarget t) (pointCanShow <$> canShow <*> point) (marker t)
    where -- update candidate point with canShow status
          pointCanShow true p  = p
          pointCanShow false _ = Nothing

          updTarget DefaultAdder = true
          updTarget CrossAdder   = false

          marker DefaultAdder = blueMarker
          marker CrossAdder   = crossMarker
