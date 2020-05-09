module Test where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Plus (empty)
import Data.Default (def)
import Data.Either (Either(..))
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Editor.Common.Lenses (_leadId, _panelType, _rackingType, _textureInfo)
import Editor.Editor (createEditor)
import Editor.EditorMode (EditorMode(..))
import Editor.SceneEvent (size)
import Editor.WebEditor (_dataServer, _elem, _modeDyn, _panels, _roofPlates, _sizeDyn, runWebEditor)
import Effect (Effect)
import Effect.Class.Console (logShow)
import FRP.Dynamic (step)
import FRP.Event (subscribe)
import FRP.Event.Extra (after)
import Foreign (Foreign)
import Foreign.Generic (decode)
import Model.Hardware.PanelTextureInfo (_premium, _standard, _standard72)
import Model.Hardware.PanelType (PanelType(..))
import Model.Racking.RackingType (RackingType(..))
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
                let modeDyn = step Showing (const RoofEditing <$> after 10000)
                    sizeDyn = step (size 800 600) empty
                    panelType = step Standard empty
                    rackingType = step XR10 empty

                    textures = def # _standard   .~ Just solarModuleJPG
                                   # _premium    .~ Just qCellSolarPanelJPG
                                   # _standard72 .~ Just qCellSolarPanel72PNG

                    cfg = def # _elem       .~ elem
                              # _leadId     .~ 296285
                              # _roofPlates .~ roofs
                              # _panels     .~ panels
                              # _dataServer .~ serverUrl
                              # _modeDyn    .~ modeDyn
                              # _sizeDyn    .~ sizeDyn
                              # _panelType  .~ panelType
                              # _rackingType .~ rackingType
                              # _textureInfo .~ textures
                res <- runWebEditor cfg createEditor
                
                _ <- subscribe (fst res) logShow

                pure unit