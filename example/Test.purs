module Test where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Data.Default (def)
import Data.Either (Either(..))
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Editor.Common.Lenses (_leadId, _panelType, _textureInfo)
import Editor.Editor (createEditor)
import Editor.EditorMode (EditorMode(..))
import Editor.SceneEvent (size)
import Editor.WebEditor (_dataServer, _elem, _modeEvt, _panels, _roofPlates, _sizeEvt, runWebEditor)
import Effect (Effect)
import Effect.Class.Console (logShow)
import FRP.Event (subscribe)
import FRP.Event.Extra (after)
import Foreign (Foreign)
import Foreign.Generic (decode)
import Model.Hardware.PanelTextureInfo (_premium, _standard, _standard72)
import Model.Hardware.PanelType (PanelType(..))
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)


foreign import solarModuleJPG :: String
foreign import qCellSolarPanelJPG :: String
foreign import qCellSolarPanel72PNG :: String

serverUrl :: String
serverUrl = "https://s3.eu-west-1.amazonaws.com/data.electrious.com"

doTest :: Foreign -> Foreign -> Effect Unit
doTest roofDat panelDat = do
    w <- window
    doc <- document w
    elem <- getElementById "editor" (toNonElementParentNode doc)

    case runExcept $ decode roofDat of
        Left e -> logShow e
        Right roofs -> case runExcept $ decode panelDat of
            Left e -> logShow e
            Right panels -> do
                let modeEvt = const Showing <$> after 10
                    sizeEvt = const (size 800 600) <$> after 2
                    panelType = const Standard <$> after 10

                    textures = def # _standard   .~ Just solarModuleJPG
                                   # _premium    .~ Just qCellSolarPanelJPG
                                   # _standard72 .~ Just qCellSolarPanel72PNG

                    cfg = def # _elem       .~ elem
                              # _leadId     .~ 296285
                              # _roofPlates .~ roofs
                              # _panels     .~ panels
                              # _dataServer .~ serverUrl
                              # _modeEvt    .~ modeEvt
                              # _sizeEvt    .~ sizeEvt
                              # _panelType  .~ panelType
                              # _textureInfo .~ textures
                res <- runWebEditor cfg createEditor
                
                _ <- subscribe (fst res) logShow

                pure unit