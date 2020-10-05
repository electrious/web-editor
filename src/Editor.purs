module Editor.Editor where

import Prelude hiding (add,degree)

import Control.Plus (empty)
import Data.Array (cons)
import Data.Default (class Default, def)
import Data.Foldable (sequence_)
import Data.Int (toNumber)
import Data.Lens (Lens', view, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_deltaX, _deltaY, _height, _modeDyn, _shiftDragged, _width, _zoomed)
import Editor.Disposable (class Disposable, dispose)
import Editor.EditorMode (EditorMode(..))
import Editor.Input (setupInput)
import Editor.Input.Commoon (DragEvent)
import Editor.SceneEvent (Size, _dragEvent, setupRaycasting, size)
import Effect (Effect)
import Effect.Ref (Ref, modify, new, read)
import FRP.Dynamic (Dynamic, gateDyn, sampleDyn, step, subscribeDyn)
import FRP.Event (subscribe)
import FRP.Event.Extra (performEvent)
import Math.Angle (degree, radianVal)
import Three.Controls.OrbitControls (OrbitControls, enableDamping, enableZoom, isEnabled, mkOrbitControls, setAutoRotate, setAutoRotateSpeed, setDampingFactor, setEnabled, setMaxDistance, setMaxPolarAngle, setMinDistance, setMinPolarAngle, setTarget, update)
import Three.Controls.OrbitControls as OrbitControls
import Three.Core.Camera (class IsCamera, PerspectiveCamera, mkPerspectiveCamera, setAspect, updateProjectionMatrix)
import Three.Core.Light (mkAmbientLight, mkDirectionalLight)
import Three.Core.Object3D (class IsObject3D, Object3D, add, hasParent, lookAt, mkObject3D, parent, position, rotateOnWorldAxis, rotateZ, setDefaultUp, setName, setPosition, setRotation, translateX, translateY, worldToLocal)
import Three.Core.Scene (disposeScene, mkScene)
import Three.Core.WebGLRenderer (domElement, mkWebGLRenderer, render, setSize)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (Vector3, length, mkVec3, multiplyScalar, normal, vecX, vecY)
import Web.DOM (Element)
import Web.DOM.Element (toNode)
import Web.DOM.Node (appendChild)
import Web.HTML (Window, window)
import Web.HTML.Window (requestAnimationFrame)

newtype EditorConfig = EditorConfig {
    sizeDyn         :: Dynamic Size,
    modeDyn         :: Dynamic EditorMode,
    flyCameraTarget :: Dynamic (Maybe Vector3)
}

derive instance newtypeEditorConfig :: Newtype EditorConfig _
instance defaultEditorConfig :: Default EditorConfig where
    def = EditorConfig {
        sizeDyn         : step (size 800 600) empty,
        modeDyn         : step Showing empty,
        flyCameraTarget : step Nothing empty
    }

_sizeDyn :: forall t a r. Newtype t { sizeDyn :: a | r } => Lens' t a
_sizeDyn = _Newtype <<< prop (SProxy :: SProxy "sizeDyn")

_flyCameraTarget :: forall t a r. Newtype t { flyCameraTarget :: a | r } => Lens' t a
_flyCameraTarget = _Newtype <<< prop (SProxy :: SProxy "flyCameraTarget")

-- | internal record that defines all components for threejs related objects
newtype Editor = Editor {
    canvas     :: Element,
    render     :: Effect Unit,
    content    :: Object3D,
    disposable :: Ref (Array (Effect Unit))
}

derive instance newtypeEditor :: Newtype Editor _
instance disposableEditor :: Disposable Editor where
    dispose (Editor { disposable }) = read disposable >>= sequence_
instance isObject3DEditor :: IsObject3D Editor where
    toObject3D = view _content

_canvas :: forall t a r. Newtype t { canvas :: a | r } => Lens' t a
_canvas = _Newtype <<< prop (SProxy :: SProxy "canvas")

_content :: forall t a r. Newtype t { content :: a | r } => Lens' t a
_content = _Newtype <<< prop (SProxy :: SProxy "content")

renderEditor :: Editor -> Effect Unit
renderEditor (Editor s) = s.render

addDisposable :: Effect Unit -> Editor -> Effect Unit
addDisposable d (Editor e) = void $ modify (cons d) e.disposable

capVal :: Number -> Number -> Number -> Number
capVal bot top v | v < bot = bot
                 | v > top = top
                 | otherwise = v

setupCameraPos :: forall c. IsCamera c => c -> Effect Unit
setupCameraPos camera = do
    setPosition (mkVec3 0.0 (-40.0) 20.0) camera
    lookAt def camera

resetContentPos :: forall a. IsObject3D a => a -> Effect Unit
resetContentPos = setPosition def

resetContentRot :: forall a. IsObject3D a => a -> Effect Unit
resetContentRot = setRotation (mkEuler 0.0 0.0 0.0)

zoomCamera :: PerspectiveCamera -> Number -> Effect Number
zoomCamera camera zoom = do
    let pos = position camera
        curDist = length pos
        newDist = capVal 5.0 500.0 (curDist + zoom)
        newPos = multiplyScalar (normal pos) newDist
    setPosition newPos camera
    pure newDist


rotateContentWithDrag :: forall a. IsObject3D a => a -> DragEvent -> Effect Unit
rotateContentWithDrag obj drag = do
    let dx = drag ^. _deltaX / 360.0
    rotateZ dx obj
    rotateOnWorldAxis (mkVec3 1.0 0.0 0.0) ((drag ^. _deltaY) / 360.0) obj

moveWithShiftDrag :: forall a. IsObject3D a => a -> DragEvent -> Number -> Effect Unit
moveWithShiftDrag obj drag scale | not (hasParent obj) = pure unit
                                 | otherwise = do
                                    let p = parent obj
                                        vec = mkVec3 (drag ^. _deltaX) (- (drag ^. _deltaY)) 0.0
                                    lVec <- worldToLocal vec (p :: Object3D)
                                    translateX (vecX lVec * scale / 10.0) obj
                                    translateY (vecY lVec * scale / 10.0) obj

setupOrbitControls :: OrbitControls -> Dynamic (Maybe Vector3) -> Effect (Effect Unit)
setupOrbitControls c target = do
    setAutoRotate true c
    setAutoRotateSpeed 0.5 c
    enableDamping true c
    setDampingFactor 0.1 c
    enableZoom true c
    setMinPolarAngle (radianVal $ degree 10.0) c
    setMaxPolarAngle (radianVal $ degree 50.0) c
    setMinDistance 15.0 c
    setMaxDistance 35.0 c
    let t = fromMaybe (mkVec3 0.0 0.0 (-5.0)) <$> target
    subscribeDyn t (flip setTarget c)

-- | internal function to create the threejs scene, camera, light and renderer
createEditor :: Element -> EditorConfig -> Effect Editor
createEditor elem cfg = do
    -- set the default Up direction as z axis in the scene
    setDefaultUp (mkVec3 0.0 0.0 1.0)

    scene    <- mkScene
    camera   <- mkPerspectiveCamera 45.0 (800.0 / 600.0) 0.1 1000.0
    renderer <- mkWebGLRenderer

    -- function to update camera and renderer when resized
    let resized s = do
            setAspect (toNumber (s ^. _width) / toNumber (s ^. _height)) camera
            updateProjectionMatrix camera
            setSize (s ^. _width) (s ^. _height) renderer
        
        sizeDyn = cfg ^. _sizeDyn
    
    d1 <- subscribeDyn sizeDyn resized

    -- attach the webgl canvas to parent DOM element
    let canvas = domElement renderer
    _ <- appendChild (toNode canvas) (toNode elem)

    -- set the camera position and orient it toward the center
    setupCameraPos camera

    let cameraDefDist = length (position camera)

    -- setup the orbit controls
    orbitCtrl <- mkOrbitControls camera (domElement renderer)
    d5 <- setupOrbitControls orbitCtrl $ cfg ^. _flyCameraTarget

    let isShowing = (==) Showing <$> (cfg ^. _modeDyn)
        canEdit = not <$> isShowing

    -- add ambient light
    ambientLight <- mkAmbientLight 0xffffff
    setName "ambient-light" ambientLight
    add ambientLight scene

    -- add a directional light
    dirLight <- mkDirectionalLight 0xeeeeee 0.5
    setName "directional-light" dirLight
    setPosition (mkVec3 100.0 0.0 100.0) dirLight
    add dirLight scene

    -- add a wrapper object that's used only to rotate the scene around
    rotWrapper <- mkObject3D
    setName "rotate-wrapper" rotWrapper
    add rotWrapper scene

    -- add a wrapper object to be parent of all user contents
    content <- mkObject3D
    setName "scene-content" content
    add content rotWrapper

    d4 <- subscribeDyn isShowing $ \s -> do
             if s
                then resetContentPos content *> resetContentRot rotWrapper
                else setupCameraPos camera
             setEnabled s orbitCtrl

    -- function to update renderring of the webgl scene
    let renderFunc = do
            render scene camera renderer
            when (isEnabled orbitCtrl) $ update orbitCtrl
    
        inputEvts = setupInput (domElement renderer)
    
        newDistEvt = performEvent $ zoomCamera camera <$> (gateDyn canEdit $ inputEvts ^. _zoomed)
        scaleEvt = (\d -> d / cameraDefDist) <$> newDistEvt
        scaleDyn = step 1.0 scaleEvt
    rcs <- setupRaycasting camera scene inputEvts sizeDyn

    d2 <- subscribe (gateDyn canEdit (rcs ^. _dragEvent)) (rotateContentWithDrag rotWrapper)

    let shiftDragEvt = performEvent $ sampleDyn scaleDyn $ moveWithShiftDrag content <$> gateDyn canEdit (inputEvts ^. _shiftDragged) 
    d3 <- subscribe shiftDragEvt (const $ pure unit)

    disposable <- new [d1, d2, d3, d4, d5, disposeScene scene, dispose rcs, OrbitControls.dispose orbitCtrl]
    let editor = Editor {
        canvas     : canvas,
        render     : renderFunc,
        content    : content,
        disposable : disposable
    }

    -- start the render loop
    window >>= renderLoop editor
    
    pure editor

-- | renderLoop is the function to render scene repeatedly
renderLoop :: Editor -> Window -> Effect Unit
renderLoop editor w = do
    _ <- requestAnimationFrame (renderLoop editor w) w
    renderEditor editor
