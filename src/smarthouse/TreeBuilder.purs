module SmartHouse.TreeBuilder where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (empty)
import Control.Monad.Reader.Class (ask)
import Custom.Mesh (TappableMesh)
import Data.Default (class Default, def)
import Data.Lens (set, view, (.~), (^.))
import Data.Maybe (Maybe(..))
import Data.Meter (Meter, meter, meterVal)
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_face, _height, _isActive, _name, _parent, _point, _position, _rotation, _tapped, _updated)
import Editor.ObjectAdder (AdderType(..), createObjectAdder, mkCandidatePoint)
import Editor.SceneEvent (SceneMouseMoveEvent)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, distinctDyn, latestEvt, sampleDyn, step)
import FRP.Event (Event)
import FRP.Event.Extra (multicast, performEvent)
import Math (pi)
import Math.Line (_direction)
import Model.ActiveMode (ActiveMode, isActive)
import Model.SmartHouse.Tree (Tree, TreeNode, TreeOp(..), TreePart, _barrel, _canopy, _crown, _dia, mkTree)
import Model.UUID (idLens)
import Rendering.DynamicNode (dynamic)
import Rendering.Node (Node, _renderOrder, _visible, fixNodeDWith, mesh, node, tapMesh)
import Three.Core.Face3 (normal)
import Three.Core.Geometry (BufferGeometry, CircleGeometry, mkCircleGeometry, mkCylinderGeometry)
import Three.Core.Material (MeshBasicMaterial, MeshPhongMaterial, mkMeshBasicMaterial, mkMeshPhongMaterial, setOpacity)
import Three.Core.Object3D (worldToLocal)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (Vector3, mkVec3, vecX, vecY, vecZ)
import UI.DraggableObject (DragObjCfg, _customGeo, _deltaTransform, _validator, createDraggableObject)


trunkMat :: MeshBasicMaterial
trunkMat = unsafePerformEffect do
    mat <- mkMeshBasicMaterial 0x512e1c
    setOpacity 0.7 mat
    pure mat


leafMat :: MeshPhongMaterial
leafMat = unsafePerformEffect $ mkMeshPhongMaterial 0x3cb200


getTapEvt :: TappableMesh -> Event Unit
getTapEvt = map (const unit) <<< view _tapped

buildTrunk :: forall e. Dynamic Meter -> Node e (Event Unit)
buildTrunk = map latestEvt <<< dynamic <<< map mkT
    where mkT h = do let ht = meterVal h * 0.8
                     geo <- liftEffect $ mkCylinderGeometry 0.3 0.7 ht 20 false
                     getTapEvt <$> tapMesh (def # _name        .~ "tree-trunk"
                                                # _position    .~ pure (mkVec3 0.0 0.0 (ht / 2.0))
                                                # _rotation    .~ pure (mkEuler (pi / 2.0) 0.0 0.0)
                                                # _renderOrder .~ 6
                                           ) geo trunkMat


buildCrown :: forall e. Dynamic Meter -> Dynamic TreePart -> Node e (Event Unit)
buildCrown hDyn crownDyn = latestEvt <$> dynamic (mkC <$> hDyn <*> crownDyn)
    where mkC height c = do
              let th = meterVal height
                  ch = meterVal $ c ^. _height
                  cd = meterVal $ c ^. _dia

                  h = th - ch

                  pos = mkVec3 0.0 0.0 (ch + h / 2.0)
                  
              geo <- liftEffect $ mkCylinderGeometry 0.0 cd h 40 true
              getTapEvt <$> tapMesh (def # _name        .~ "tree-crown"
                                         # _position    .~ pure pos
                                         # _rotation    .~ pure (mkEuler (pi / 2.0) 0.0 0.0)
                                         # _renderOrder .~ 9
                                    ) geo leafMat


-- build the tree barrel part
buildBarrel :: forall e. Dynamic TreePart -> Dynamic TreePart -> Node e (Event Unit)
buildBarrel crownDyn barrelDyn = latestEvt <$> dynamic (mkB <$> crownDyn <*> barrelDyn)
    where mkB c b = do
              let ch = meterVal $ c ^. _height
                  cd = meterVal $ c ^. _dia
                  bh = meterVal $ b ^. _height
                  bd = meterVal $ b ^. _dia
                  
                  h = ch - bh

                  pos = mkVec3 0.0 0.0 (bh + h / 2.0)
                  
              geo <- liftEffect $ mkCylinderGeometry cd bd h 40 true
              getTapEvt <$> tapMesh (def # _name        .~ "tree-barrel"
                                         # _position    .~ pure pos
                                         # _rotation    .~ pure (mkEuler (pi / 2.0) 0.0 0.0)
                                         # _renderOrder .~ 8
                                    ) geo leafMat


-- build the tree canopy part
buildCanopy :: forall e. Dynamic TreePart -> Dynamic TreePart -> Node e (Event Unit)
buildCanopy barrelDyn canopyDyn = latestEvt <$> dynamic (mkC <$> barrelDyn <*> canopyDyn)
    where mkC b c = do
              let bh = meterVal $ b ^. _height
                  bd = meterVal $ b ^. _dia
                  ch = meterVal $ c ^. _height
                  cd = meterVal $ c ^. _dia

                  h = bh - ch

                  pos = mkVec3 0.0 0.0 (ch + h / 2.0)
                  cPos = mkVec3 0.0 0.0 ch
              
              -- bottom cap under the canopy
              circGeo <- liftEffect $ mkCircleGeometry cd 40
              void $ mesh (def # _position .~ pure cPos
                               # _rotation .~ pure (mkEuler pi 0.0 0.0)) circGeo leafMat

              geo <- liftEffect $ mkCylinderGeometry bd cd h 40 true
              getTapEvt <$> tapMesh (def # _name        .~ "tree-canopy"
                                         # _position    .~ pure pos
                                         # _rotation    .~ pure (mkEuler (pi / 2.0) 0.0 0.0)
                                         # _renderOrder .~ 7
                                    ) geo leafMat


heightBtn :: forall e. Dynamic Boolean -> Tree -> Dynamic TreePart -> Dynamic Vector3 -> Node e (Event Meter)
heightBtn actDyn tree crownDyn posDyn = node (def # _position .~ posDyn) $ view _height <$> buildTreeDragBtn actDyn cfg
    where h = tree ^. _height
          validF crown p = let z = vecZ p
                           in z < 50.0 && z > meterVal (crown ^. _height)
          cfg = def # _height    .~ h
                    # _validator .~ (validF <$> crownDyn)
                    # _direction .~ ZOnly

crownBtn :: forall e. Dynamic Boolean -> Tree -> Dynamic Meter -> Dynamic TreePart -> Dynamic Vector3 -> Node e TreeBtnEvts
crownBtn actDyn tree hDyn barrelDyn posDyn = node (def # _position .~ posDyn) $ buildTreeDragBtn actDyn cfg
    where c = tree ^. _crown
          validF h barrel p = let x = vecX p
                                  z = vecZ p
                              in x > 0.0 && x < 20.0 && z > meterVal (barrel ^. _height) && z < meterVal h
          cfg = def # _height .~ (c ^. _height)
                    # _dia    .~ (c ^. _dia)
                    # _validator .~ (validF <$> hDyn <*> barrelDyn)



barrelBtn :: forall e. Dynamic Boolean -> Tree -> Dynamic TreePart -> Dynamic TreePart -> Dynamic Vector3 -> Node e TreeBtnEvts
barrelBtn actDyn tree crownDyn canopyDyn posDyn = node (def # _position .~ posDyn) $ buildTreeDragBtn actDyn cfg
    where b = tree ^. _barrel
          validF crown canopy p = let x = vecX p
                                      z = vecZ p
                                  in x > 0.0 && x < 20.0 && z > meterVal (canopy ^. _height) && z < meterVal (crown ^. _height)
          cfg = def # _height .~ (b ^. _height)
                    # _dia    .~ (b ^. _dia)
                    # _validator .~ (validF <$> crownDyn <*> canopyDyn)


canopyBtn :: forall e. Dynamic Boolean -> Tree -> Dynamic TreePart -> Dynamic Vector3 -> Node e TreeBtnEvts
canopyBtn actDyn tree barrelDyn posDyn = node (def # _position .~ posDyn) $ buildTreeDragBtn actDyn cfg
    where c = tree ^. _canopy
          validF barrel p = let x = vecX p
                                z = vecZ p
                            in x > 0.0 && x < 20.0 && z > 0.0 && z < meterVal (barrel ^. _height)
          cfg = def # _height .~ (c ^. _height)
                    # _dia    .~ (c ^. _dia)
                    # _validator .~ (validF <$> barrelDyn)


editTree :: forall e. Tree -> Dynamic ActiveMode -> Node e TreeNode
editTree tree actDyn = fixNodeDWith tree \treeDyn -> do
    let hDyn      = distinctDyn $ view _height   <$> treeDyn
        posDyn    = distinctDyn $ view _position <$> treeDyn
        crownDyn  = distinctDyn $ view _crown    <$> treeDyn
        barrelDyn = distinctDyn $ view _barrel   <$> treeDyn
        canopyDyn = distinctDyn $ view _canopy   <$> treeDyn

        isActDyn  = isActive <$> actDyn

    tapEvt <- node (def # _name .~ "tree"
                        # _position .~ posDyn) do
        tapTrunkEvt  <- buildTrunk hDyn
        tapCrownEvt  <- buildCrown hDyn crownDyn
        tapBarrelEvt <- buildBarrel crownDyn barrelDyn
        tapCanopyEvt <- buildCanopy barrelDyn canopyDyn

        pure $ tapTrunkEvt <|> tapCrownEvt <|> tapBarrelEvt <|> tapCanopyEvt

    hEvt       <- heightBtn isActDyn tree crownDyn posDyn
    crownEvts  <- crownBtn isActDyn tree hDyn barrelDyn posDyn
    barrelEvts <- barrelBtn isActDyn tree crownDyn canopyDyn posDyn
    canopyEvts <- canopyBtn isActDyn tree barrelDyn posDyn
    posEvt     <- buildTreePosBtn isActDyn tree

    let updEvt = (set _height               <$> hEvt)                  <|>
                 (set (_crown  <<< _height) <$> crownEvts  ^. _height) <|>
                 (set (_crown  <<< _dia)    <$> crownEvts  ^. _dia)    <|>
                 (set (_barrel <<< _height) <$> barrelEvts ^. _height) <|>
                 (set (_barrel <<< _dia)    <$> barrelEvts ^. _dia)    <|>
                 (set (_canopy <<< _height) <$> canopyEvts ^. _height) <|>
                 (set (_canopy <<< _dia)    <$> canopyEvts ^. _dia)    <|>
                 (set _position             <$> posEvt)
                
        treeEvt = sampleDyn treeDyn updEvt

        opEvt = TreeOpUpdate <$> treeEvt

        tid = tree ^. idLens

        tn = def # idLens   .~ tid
                # _tapped  .~ (const tid <$> tapEvt)
                # _updated .~ opEvt

    pure { input: treeEvt, output : tn }


-- show a Button to let user to add a new tree
buildTree :: forall e. Dynamic ActiveMode -> Event SceneMouseMoveEvent -> Node e (Event Tree)
buildTree actDyn mouseEvt = node (def # _name .~ "tree-builder"
                                      # _visible .~ (isActive <$> actDyn)) do
    e <- ask
    let parent = e ^. _parent

        -- get a candidate point
        getCandPoint evt = do
            np <- worldToLocal (evt ^. _point) parent
            pure $ Just $ mkCandidatePoint np (normal $ evt ^. _face)
        
        candPntDyn = step Nothing $ performEvent $ getCandPoint <$> mouseEvt
    
        opt = def # _name .~ "tree-adder"
                  # _position .~ pure (mkVec3 0.0 0.0 0.01)

    addedPntEvt <- node opt $ createObjectAdder DefaultAdder candPntDyn (isActive <$> actDyn)

    pure $ performEvent $ mkTree <<< view _position <$> addedPntEvt


data DragDirection = XZ
                   | ZOnly

derive instance eqDragDirection :: Eq DragDirection

newtype TreeDragBtn = TreeDragBtn {
    position  :: Vector3,
    height    :: Meter,
    dia       :: Meter,
    validator :: Dynamic (Vector3 -> Boolean),
    direction :: DragDirection
    }

derive instance newtypeTreeDragBtn :: Newtype TreeDragBtn _
instance defaultTreeDragBtn :: Default TreeDragBtn where
    def = TreeDragBtn {
        position  : def,
        height    : def,
        dia       : def,
        validator : pure (const true),
        direction : XZ
        }


newtype TreeBtnEvts = TreeBtnEvts {
    height :: Event Meter,
    dia    :: Event Meter
}

derive instance newtypeTreeBtnEvts :: Newtype TreeBtnEvts _
instance defaultTreeBtnEvts :: Default TreeBtnEvts where
    def = TreeBtnEvts {
        height : empty,
        dia    : empty
        }

buildTreeDragBtn :: forall e. Dynamic Boolean -> TreeDragBtn -> Node e TreeBtnEvts
buildTreeDragBtn actDyn cfg = do
    let h = cfg ^. _height
        d = cfg ^. _dia

        -- make sure the arrow can only be dragged along X and Z axis
        transF v = mkVec3 (vecX v) 0.0 (vecZ v)
        transF2 v = mkVec3 0.0 0.0 (vecZ v)

        dragCfg :: DragObjCfg BufferGeometry
        dragCfg = def # _isActive       .~ actDyn
                      # _position       .~ mkVec3 (meterVal d) (meterVal h) 0.0
                      # _rotation       .~ mkEuler (pi / 2.0) 0.0 0.0
                      # _validator      .~ (cfg ^. _validator)
                      # _deltaTransform .~ Just (if cfg ^. _direction == XZ then transF else transF2)

    btn <- createDraggableObject dragCfg
    let posEvt = multicast $ btn ^. _position
        
    pure $ def # _height .~ (meter <<< vecZ <$> posEvt)
               # _dia    .~ (meter <<< vecX <$> posEvt)


-- big button to drag the tree around
posBtnGeo :: CircleGeometry
posBtnGeo = unsafePerformEffect $ mkCircleGeometry 2.0 30

-- button to move the tree around
buildTreePosBtn :: forall e. Dynamic Boolean -> Tree -> Node e (Event Vector3)
buildTreePosBtn actDyn tree = do
    let transF v = mkVec3 (vecX v) (vecY v) 0.0

        dragCfg :: DragObjCfg CircleGeometry
        dragCfg = def # _isActive       .~ actDyn
                      # _position       .~ (tree ^. _position)
                      # _deltaTransform .~ Just transF
                      # _customGeo      .~ Just posBtnGeo

    btn <- createDraggableObject dragCfg
    pure $ transF <$> btn ^. _position
