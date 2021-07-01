module SmartHouse.TreeBuilder where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (empty)
import Data.Default (class Default, def)
import Data.Lens (set, view, (.~), (^.))
import Data.Maybe (Maybe(..))
import Data.Meter (Meter, meter, meterVal)
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_height, _isActive, _name, _position, _rotation)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, distinctDyn, sampleDyn)
import FRP.Event (Event)
import Math (pi)
import Math.Line (_direction)
import Model.SmartHouse.Tree (Tree, TreePart, _barrel, _canopy, _crown, _dia)
import Rendering.DynamicNode (dynamic_)
import Rendering.Node (Node, _renderOrder, fixNodeDWith, mesh)
import Three.Core.Geometry (BufferGeometry, mkCylinderGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial, setOpacity)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (Vector3, mkVec3, vecX, vecZ)
import UI.DraggableObject (DragObjCfg, _deltaTransform, _validator, createDraggableObject)


trunkMat :: MeshBasicMaterial
trunkMat = unsafePerformEffect do
    mat <- mkMeshBasicMaterial 0x512e1c
    setOpacity 0.7 mat
    pure mat


leafMat :: MeshBasicMaterial
leafMat = unsafePerformEffect $ mkMeshBasicMaterial 0x3cb200


buildTrunk :: forall e. Dynamic Meter -> Node e Unit
buildTrunk = dynamic_ <<< map mkT
    where mkT h = do let ht = meterVal h * 0.8
                     geo <- liftEffect $ mkCylinderGeometry 0.1 0.1 ht 20
                     mesh (def # _name        .~ "tree-trunk"
                               # _position    .~ pure (mkVec3 0.0 0.0 (ht / 2.0))
                               # _renderOrder .~ 6
                          ) geo trunkMat


buildCrown :: forall e. Dynamic Meter -> Dynamic TreePart -> Node e Unit
buildCrown hDyn crownDyn = dynamic_ $ mkC <$> hDyn <*> crownDyn
    where mkC height c = do
              let th = meterVal height
                  ch = meterVal $ c ^. _height
                  cd = meterVal $ c ^. _dia

                  h = th - ch

                  pos = mkVec3 0.0 0.0 (ch + h / 2.0)
                  
              geo <- liftEffect $ mkCylinderGeometry 0.0 cd h 20
              mesh (def # _name        .~ "tree-crown"
                        # _position    .~ pure pos
                        # _renderOrder .~ 9
                   ) geo leafMat


-- build the tree barrel part
buildBarrel :: forall e. Dynamic TreePart -> Dynamic TreePart -> Node e Unit
buildBarrel crownDyn barrelDyn = dynamic_ $ mkB <$> crownDyn <*> barrelDyn
    where mkB c b = do
              let ch = meterVal $ c ^. _height
                  cd = meterVal $ c ^. _dia
                  bh = meterVal $ b ^. _height
                  bd = meterVal $ b ^. _dia
                  
                  h = ch - bh

                  pos = mkVec3 0.0 0.0 (bh + h / 2.0)
                  
              geo <- liftEffect $ mkCylinderGeometry cd bd h 20
              mesh (def # _name        .~ "tree-barrel"
                        # _position    .~ pure pos
                        # _renderOrder .~ 8
                   ) geo leafMat


-- build the tree canopy part
buildCanopy :: forall e. Dynamic TreePart -> Dynamic TreePart -> Node e Unit
buildCanopy barrelDyn canopyDyn = dynamic_ $ mkC <$> barrelDyn <*> canopyDyn
    where mkC b c = do
              let bh = meterVal $ b ^. _height
                  bd = meterVal $ b ^. _dia
                  ch = meterVal $ c ^. _height
                  cd = meterVal $ c ^. _dia

                  h = bh - ch

                  pos = mkVec3 0.0 0.0 (ch + h / 2.0)
                  
              geo <- liftEffect $ mkCylinderGeometry bd cd h 20
              mesh (def # _name        .~ "tree-canopy"
                        # _position    .~ pure pos
                        # _renderOrder .~ 7
                   ) geo leafMat


heightBtn :: forall e. Tree -> Dynamic TreePart -> Node e (Event Meter)
heightBtn tree canopyDyn = view _height <$> buildTreeDragBtn cfg
    where h = tree ^. _height
          validF canopy p = let z = vecZ p
                            in z < 50.0 && z > meterVal (canopy ^. _height)
          cfg = def # _height    .~ h
                    # _validator .~ (validF <$> canopyDyn)
                    # _direction .~ ZOnly

crownBtn :: forall e. Tree -> Dynamic Meter -> Dynamic TreePart -> Node e TreeBtnEvts
crownBtn tree hDyn barrelDyn = buildTreeDragBtn cfg
    where c = tree ^. _crown
          validF h barrel p = let x = vecX p
                                  z = vecZ p
                              in x > 0.0 && x < 20.0 && z > meterVal (barrel ^. _height) && z < meterVal h
          cfg = def # _height .~ (c ^. _height)
                    # _dia    .~ (c ^. _dia)
                    # _validator .~ (validF <$> hDyn <*> barrelDyn)



barrelBtn :: forall e. Tree -> Dynamic TreePart -> Dynamic TreePart -> Node e TreeBtnEvts
barrelBtn tree crownDyn canopyDyn = buildTreeDragBtn cfg
    where b = tree ^. _barrel
          validF crown canopy p = let x = vecX p
                                      z = vecZ p
                                  in x > 0.0 && x < 20.0 && z > meterVal (canopy ^. _height) && z < meterVal (crown ^. _height)
          cfg = def # _height .~ (b ^. _height)
                    # _dia    .~ (b ^. _dia)
                    # _validator .~ (validF <$> crownDyn <*> canopyDyn)


canopyBtn :: forall e. Tree -> Dynamic TreePart -> Node e TreeBtnEvts
canopyBtn tree barrelDyn = buildTreeDragBtn cfg
    where c = tree ^. _canopy
          validF barrel p = let x = vecX p
                                z = vecZ p
                            in x > 0.0 && x < 20.0 && z > 0.0 && z < meterVal (barrel ^. _height)
          cfg = def # _height .~ (c ^. _height)
                    # _dia    .~ (c ^. _dia)
                    # _validator .~ (validF <$> barrelDyn)


editTree :: forall e. Tree -> Node e Unit
editTree tree = fixNodeDWith tree \treeDyn -> do

    let hDyn      = distinctDyn $ view _height <$> treeDyn
        crownDyn  = distinctDyn $ view _crown <$> treeDyn
        barrelDyn = distinctDyn $ view _barrel <$> treeDyn
        canopyDyn = distinctDyn $ view _canopy <$> treeDyn

    buildTrunk hDyn
    buildCrown hDyn crownDyn
    buildBarrel crownDyn barrelDyn
    buildCanopy barrelDyn canopyDyn

    hEvt       <- heightBtn tree canopyDyn
    crownEvts  <- crownBtn tree hDyn barrelDyn
    barrelEvts <- barrelBtn tree crownDyn canopyDyn
    canopyEvts <- canopyBtn tree barrelDyn

    let updEvt = (set _height               <$> hEvt)                  <|>
                 (set (_crown  <<< _height) <$> crownEvts  ^. _height) <|>
                 (set (_crown  <<< _dia)    <$> crownEvts  ^. _dia)    <|>
                 (set (_barrel <<< _height) <$> barrelEvts ^. _height) <|>
                 (set (_barrel <<< _dia)    <$> barrelEvts ^. _dia)    <|>
                 (set (_canopy <<< _height) <$> canopyEvts ^. _height) <|>
                 (set (_canopy <<< _dia)    <$> canopyEvts ^. _dia)
                 
        treeEvt = sampleDyn treeDyn updEvt
    
    pure { input: treeEvt, output : unit }



data DragDirection = XZ
                   | ZOnly

derive instance eqDragDirection :: Eq DragDirection
                     
newtype TreeDragBtn = TreeDragBtn {
    height    :: Meter,
    dia       :: Meter,
    validator :: Dynamic (Vector3 -> Boolean),
    direction :: DragDirection
    }

derive instance newtypeTreeDragBtn :: Newtype TreeDragBtn _
instance defaultTreeDragBtn :: Default TreeDragBtn where
    def = TreeDragBtn {
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

buildTreeDragBtn :: forall e. TreeDragBtn -> Node e TreeBtnEvts
buildTreeDragBtn cfg = do
    let h = cfg ^. _height
        d = cfg ^. _dia

        -- make sure the arrow can only be dragged along X and Z axis
        transF v = mkVec3 (vecX v) 0.0 (vecZ v)
        transF2 v = mkVec3 0.0 0.0 (vecZ v)
        
        
        dragCfg :: DragObjCfg BufferGeometry
        dragCfg = def # _isActive       .~ pure true
                      # _position       .~ mkVec3 (meterVal d) 0.0 (meterVal h)
                      # _rotation       .~ mkEuler (pi / 2.0) 0.0 0.0
                      # _validator      .~ (cfg ^. _validator)
                      # _deltaTransform .~ Just (if cfg ^. _direction == XZ then transF else transF2)

    btn <- createDraggableObject dragCfg
    let posEvt = btn ^. _position
        
    pure $ def # _height .~ (meter <<< vecZ <$> posEvt)
               # _dia    .~ (meter <<< vecX <$> posEvt)
