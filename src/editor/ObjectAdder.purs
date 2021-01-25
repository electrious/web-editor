module Editor.ObjectAdder where

import Prelude

import Data.Compactable (compact)
import Data.Default (def)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Editor.Common.Lenses (_name, _position, _tapped)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, performDynamic, sampleDyn_)
import FRP.Event (Event)
import Rendering.Node (Node, _target, _visible, getParent, tapMesh)
import Three.Core.Geometry (CircleGeometry, mkCircleGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial)
import Three.Core.Object3D (localToWorld)
import Three.Math.Vector (class Vector, Vector3, addScaled, getVector, (<+>))


-- | Candidate point that will allow user to show the adder marker
newtype CandidatePoint v = CandidatePoint {
    position   :: v,
    faceNormal :: Vector3
}

derive instance newtypeCandidatePoint :: Newtype (CandidatePoint v) _

_faceNormal :: forall t a r. Newtype t { faceNormal :: a | r } => Lens' t a
_faceNormal = _Newtype <<< prop (SProxy :: SProxy "faceNormal")

mkCandidatePoint :: forall v. v -> Vector3 -> CandidatePoint v
mkCandidatePoint p n = CandidatePoint { position : p, faceNormal : n }

adderMarkerMat :: MeshBasicMaterial
adderMarkerMat = unsafePerformEffect (mkMeshBasicMaterial 0x2222ff)

adderMarkerGeo :: CircleGeometry
adderMarkerGeo = unsafePerformEffect (mkCircleGeometry 1.0 32)

createAdderMarker :: forall e v. Vector v => Dynamic (Maybe (CandidatePoint v)) -> Node e (Event (CandidatePoint v))
createAdderMarker pDyn = do
    parent <- getParent

    let posV p = getVector $ p ^. _position
        -- get the local position of the candidate point
        -- and move it along the normal vector a bit.
        -- then used as the new position of the marker
        calcPos Nothing  = def
        calcPos (Just p) = addScaled (posV p) (p ^. _faceNormal) 0.03

        calcTarget p = localToWorld (posV p <+> p ^. _faceNormal) parent

        posDyn    = calcPos <$> pDyn
        targetDyn = performDynamic $ traverse calcTarget <$> pDyn
        
    m <- tapMesh (def # _name     .~ "adder-marker"
                      # _position .~ posDyn
                      # _target   .~ targetDyn
                      # _visible  .~ (isJust <$> pDyn)
                 ) adderMarkerGeo adderMarkerMat
    pure $ compact $ sampleDyn_ pDyn $ m ^. _tapped

-- | create a object adder
createObjectAdder :: forall e v. Vector v => Dynamic (Maybe (CandidatePoint v)) -> Dynamic Boolean -> Node e (Event (CandidatePoint v))
createObjectAdder point canShow = createAdderMarker $ pointCanShow <$> canShow <*> point
    where -- update candidate point with canShow status
          pointCanShow true p  = p
          pointCanShow false _ = Nothing
