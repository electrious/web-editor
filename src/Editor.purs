module Editor.Editor (EditorConfig, _elem, _defSize, _sizeEvt, _defMode, _modeEvt,
    _leadId, _roofPlates, _panels, _dataServer, _textures, WebEditorState,
    WebEditor, runWebEditor, createEditor) where

import Prelude hiding (add)

import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (class MonadState, StateT, get, modify_, runStateT)
import Control.Plus (empty)
import Control.Alt ((<|>))
import Data.Array (cons)
import Data.Default (class Default, def)
import Data.Foldable (sequence_)
import Data.Int (toNumber)
import Data.Lens (Lens', (%~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple, fst)
import Editor.ArrayBuilder (PanelTextureInfo)
import Editor.Disposable (class Disposable, dispose)
import Editor.EditorMode (EditorMode(..))
import Editor.House (loadHouseModel)
import Editor.Input (DragEvent, _deltaX, _deltaY, _shiftDragged, _zoomed, setupInput)
import Editor.RoofManager (_editedRoofs, _roofWrapper, createRoofManager)
import Editor.SceneEvent (Size, _dragEvent, _height, _width, setupRaycasting, size)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (errorShow)
import FRP.Event (Event, keepLatest, makeEvent, sampleOn, subscribe)
import FRP.Event.Extra (after, performEvent)
import Model.Roof.Panel (Panel)
import Model.Roof.RoofPlate (RoofEdited, RoofPlate)
import Three.Core.Camera (PerspectiveCamera, mkPerspectiveCamera, setAspect, updateProjectionMatrix)
import Three.Core.Light (mkAmbientLight, mkDirectionalLight)
import Three.Core.Object3D (Object3D, add, hasParent, lookAt, mkObject3D, parent, position, rotateOnWorldAxis, rotateZ, setName, setPosition, translateX, translateY, worldToLocal)
import Three.Core.Scene (disposeScene, mkScene)
import Three.Core.WebGLRenderer (domElement, mkWebGLRenderer, render, setSize)
import Three.Math.Vector (length, mkVec3, multiplyScalar, normal, vecX, vecY)
import Web.DOM (Element)
import Web.DOM.Element (toNode)
import Web.DOM.Node (appendChild)
import Web.HTML (Window, window)
import Web.HTML.Window (requestAnimationFrame)
import Web.UIEvent.WheelEvent (deltaY)

newtype EditorConfig = EditorConfig {
    elem       :: Maybe Element,
    defSize    :: Size,
    sizeEvt    :: Event Size,
    defMode    :: EditorMode,
    modeEvt    :: Event EditorMode,
    leadId     :: Int,
    roofPlates :: Array RoofPlate,
    panels     :: Array Panel,
    dataServer :: String,
    textures   :: PanelTextureInfo
}

derive instance newtypeEditorConfig :: Newtype EditorConfig _
instance defaultEditorConfig :: Default EditorConfig where
    def = EditorConfig {
        elem       : Nothing,
        defSize    : size 800 600,
        sizeEvt    : empty,
        defMode    : Showing,
        modeEvt    : empty,
        leadId     : 0,
        roofPlates : [],
        panels     : [],
        dataServer : "",
        textures   : def
    }

_elem :: Lens' EditorConfig (Maybe Element)
_elem = _Newtype <<< prop (SProxy :: SProxy "elem")

_defSize :: Lens' EditorConfig Size
_defSize = _Newtype <<< prop (SProxy :: SProxy "defSize")

_sizeEvt :: Lens' EditorConfig (Event Size)
_sizeEvt = _Newtype <<< prop (SProxy :: SProxy "sizeEvt")

_defMode :: Lens' EditorConfig EditorMode
_defMode = _Newtype <<< prop (SProxy :: SProxy "defMode")

_modeEvt :: Lens' EditorConfig (Event EditorMode)
_modeEvt = _Newtype <<< prop (SProxy :: SProxy "modeEvt")

_leadId :: Lens' EditorConfig Int
_leadId = _Newtype <<< prop (SProxy :: SProxy "leadId")

_roofPlates :: Lens' EditorConfig (Array RoofPlate)
_roofPlates = _Newtype <<< prop (SProxy :: SProxy "roofPlates")

_panels :: Lens' EditorConfig (Array Panel)
_panels = _Newtype <<< prop (SProxy :: SProxy "panels")

_dataServer :: Lens' EditorConfig String
_dataServer = _Newtype <<< prop (SProxy :: SProxy "dataServer")

_textures :: Lens' EditorConfig PanelTextureInfo
_textures = _Newtype <<< prop (SProxy :: SProxy "textures")

-- | Public interface for the main WebEditor
newtype WebEditorState = WebEditorState {
    disposables :: Array (Effect Unit)
}

derive instance newtypeWebEditorState :: Newtype WebEditorState _
instance defaultWebEditorState :: Default WebEditorState where
    def = WebEditorState {
        disposables : []
    }
instance disposableEditorState :: Disposable WebEditorState where
    dispose (WebEditorState { disposables }) = sequence_ disposables

_disposables :: Lens' WebEditorState (Array (Effect Unit))
_disposables = _Newtype <<< prop (SProxy :: SProxy "disposables")

newtype WebEditor a = WebEditor (ReaderT EditorConfig (StateT WebEditorState Effect) a)

derive newtype instance functorWebEditor     :: Functor WebEditor
derive newtype instance applyWebEditor       :: Apply WebEditor
derive newtype instance applicativeWebEditor :: Applicative WebEditor
derive newtype instance bindWebEditor        :: Bind WebEditor
derive newtype instance monadWebEditor       :: Monad WebEditor
derive newtype instance monadEffectWebEditor :: MonadEffect WebEditor
derive newtype instance monadAskWebEditor    :: MonadAsk EditorConfig WebEditor
derive newtype instance monadReaderWebEditor :: MonadReader EditorConfig WebEditor
derive newtype instance monadStateWebEditor  :: MonadState WebEditorState WebEditor

runWebEditor :: forall a. EditorConfig -> WebEditor a -> Effect (Tuple a WebEditorState)
runWebEditor cfg editor = runWebEditor' cfg def editor

runWebEditor' :: forall a. EditorConfig -> WebEditorState -> WebEditor a -> Effect (Tuple a WebEditorState)
runWebEditor' cfg st (WebEditor editor) = runStateT (runReaderT editor cfg) st

evalWebEditor' :: forall a. EditorConfig -> WebEditorState -> WebEditor a -> Effect a
evalWebEditor' cfg st editor = fst <$> runWebEditor' cfg st editor

performEditorEvent :: forall a. Event (WebEditor a) -> WebEditor (Event a)
performEditorEvent e = do
    cfg <- ask
    st  <- get

    pure $ makeEvent \k -> subscribe e (\v -> evalWebEditor' cfg st v >>= k)


addDisposable :: Effect Unit -> WebEditor Unit
addDisposable d = modify_ (\s -> s # _disposables %~ (cons d))

-- | internal record that defines all components for threejs related objects
newtype EditorScene a = EditorScene {
    render     :: Effect Unit,
    addContent :: Object3D a -> Effect Unit,
    disposable :: Effect Unit
}

derive instance newtypeEditorScene :: Newtype (EditorScene a) _
instance disposableEditorScene :: Disposable (EditorScene a) where
    dispose (EditorScene { disposable }) = disposable

capVal :: Number -> Number -> Number -> Number
capVal bot top v | v < bot = bot
                 | v > top = top
                 | otherwise = v

zoomCamera :: forall a. PerspectiveCamera a -> Number -> Effect Number
zoomCamera camera zoom = do
    let pos = position camera
        curDist = length pos
        newDist = capVal 5.0 500.0 (curDist + zoom)
        newPos = multiplyScalar (normal pos) newDist
    setPosition newPos camera
    pure newDist


rotateContentWithDrag :: forall a. Object3D a -> DragEvent -> Effect Unit
rotateContentWithDrag obj drag = do
    rotateZ ((drag ^. _deltaX) / 360.0) obj
    rotateOnWorldAxis (mkVec3 1.0 0.0 0.0) ((drag ^. _deltaY) / 360.0) obj

moveWithShiftDrag :: forall a. Object3D a -> DragEvent -> Number -> Effect Unit
moveWithShiftDrag obj drag scale | not (hasParent obj) = pure unit
                                 | otherwise = do
                                    let p = parent obj
                                        vec = mkVec3 (drag ^. _deltaX) (- (drag ^. _deltaY)) 0.0
                                    lVec <- worldToLocal vec p
                                    translateX (vecX lVec * scale / 10.0) obj
                                    translateY (vecY lVec * scale / 10.0) obj

-- | internal function to create the threejs scene, camera, light and renderer
createScene :: forall a. Size -> Event Size -> Element -> Effect (EditorScene a)
createScene defSize newSizeEvt elem = do
    scene <- mkScene
    camera <- mkPerspectiveCamera 45.0 (toNumber (defSize ^. _width) / toNumber (defSize ^. _height)) 0.1 1000.0
    renderer <- mkWebGLRenderer

    -- function to update camera and renderer when resized
    let resized s = do
            setAspect (toNumber (s ^. _width) / toNumber (s ^. _height)) camera
            updateProjectionMatrix camera
            setSize (s ^. _width) (s ^. _height) renderer
    
    let sizeEvt = newSizeEvt <|> (const defSize <$> after 10)
    d1 <- subscribe sizeEvt resized

    -- attach the webgl canvas to parent DOM element
    _ <- appendChild (toNode $ domElement renderer) (toNode elem)

    -- set the camera position and orient it toward the center
    setPosition (mkVec3 0.0 (-50.0) 50.0) camera
    lookAt (mkVec3 0.0 0.0 0.0) camera

    let cameraDefDist = length (position camera)

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

    -- function to update renderring of the webgl scene
    let renderFunc = render scene camera renderer

        addContentFunc c = add c content
    
        inputEvts = setupInput elem
    
        newDistEvt = performEvent $ (zoomCamera camera <<< deltaY) <$> inputEvts ^. _zoomed
        scaleEvt = (\d -> d / cameraDefDist) <$> newDistEvt
    
    rcs <- setupRaycasting camera scene inputEvts sizeEvt

    d2 <- subscribe (rcs ^. _dragEvent) (rotateContentWithDrag rotWrapper)

    let shiftDragEvt = performEvent $ sampleOn scaleEvt (moveWithShiftDrag content <$> inputEvts ^. _shiftDragged) 
    d3 <- subscribe shiftDragEvt (const $ pure unit)

    pure $ EditorScene {
        render     : renderFunc,
        addContent : addContentFunc,
        disposable : sequence_ [d1, d2, d3, disposeScene scene, dispose rcs]
    }

-- | renderLoop is the function to render scene repeatedly
renderLoop :: Effect Unit -> Window -> Effect Unit
renderLoop render w = do
    _ <- requestAnimationFrame (renderLoop render w) w
    render

-- | createEditor will create the Web Editor instance
createEditor :: WebEditor (Event (Array RoofEdited))
createEditor = do
    cfg <- ask

    case cfg ^. _elem of
        Nothing -> errorShow "elem is not set in EditorConfig" *> pure empty
        Just elem -> do
            (EditorScene es) <- liftEffect $ createScene (cfg ^. _defSize)
                                                         (cfg ^. _sizeEvt)
                                                         elem

            -- start the rednerring
            liftEffect $ window >>= renderLoop es.render

            let f hmd = do
                    liftEffect $ es.addContent hmd.wrapper
                    mgr <- liftEffect $ createRoofManager hmd (cfg ^. _roofPlates)
                    liftEffect $ es.addContent (mgr ^. _roofWrapper)
                    addDisposable $ dispose mgr
                    pure (mgr ^. _editedRoofs)

            e <- liftEffect $ loadHouseModel (cfg ^. _dataServer) (cfg ^. _leadId)
            keepLatest <$> performEditorEvent (f <$> e)
