module Test where

import Prelude

import API (_auth, _baseUrl)
import Control.Monad.Except (runExcept)
import Data.Default (def)
import Data.Either (Either(..))
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Editor.Common.Lenses (_apiConfig, _houseId, _leadId, _modeDyn, _panelType, _panels, _textureInfo)
import Editor.Editor (_sizeDyn, createEditor)
import Editor.EditorMode (EditorMode(..))
import Editor.HouseEditor (_arrayEditParam, _dataServer, _heatmap, _heatmapTexture, _roofplates, _rotBtnTexture)
import Editor.SceneEvent (size)
import Effect (Effect)
import Effect.Class.Console (logShow)
import FRP.Event.Extra (delay)
import Foreign (Foreign)
import Foreign.Generic (decode)
import SmartHouse.HouseBuilder (buildHouse)
import Model.Hardware.PanelTextureInfo (_premium, _standard, _standard72)
import Model.Hardware.PanelType (PanelType(..))
import Model.Roof.Panel (Panel)
import Model.Roof.RoofPlate (RoofPlate)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)


foreign import solarModuleJPG :: String
foreign import qCellSolarPanelJPG :: String
foreign import qCellSolarPanel72PNG :: String
foreign import rotateButtonPNG :: String
foreign import heatmapGradientPNG :: String

serverUrl :: String
serverUrl = "http://data.electrious.com"

doTest :: Foreign -> Foreign -> Effect Unit
doTest roofDat panelDat = do
    w <- window
    doc <- document w
    elem <- getElementById "editor" (toNonElementParentNode doc)

    case runExcept $ decode roofDat of
        Left e -> logShow e
        Right (roofs :: Array RoofPlate) -> case runExcept $ decode panelDat of
            Left e -> logShow e
            Right (panels :: Array Panel) -> case elem of
                Nothing -> logShow "can't find 'editor' element"
                Just el -> do
                    let modeDyn   = pure RoofEditing
                        sizeDyn   = pure (size 800 600)
                        panelType = pure Standard

                        textures = def # _standard   .~ Just solarModuleJPG
                                       # _premium    .~ Just qCellSolarPanelJPG
                                       # _standard72 .~ Just qCellSolarPanel72PNG
                        apiCfg = def # _auth    .~ Just "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJJZCI6IiIsImN0YyI6NCwianRpIjoiNCJ9.d6pG95A4EoAPGhhnN4BsL7QtarpBRCEcta0Uu72SoVU"
                                     # _baseUrl .~ "https://api.electrious.com/v1"

                        cfg = def # _modeDyn .~ modeDyn
                                  # _sizeDyn .~ sizeDyn
                        
                        param = def # _heatmap .~ delay 10000 (pure true)

                        houseCfg = def # _modeDyn        .~ modeDyn
                                       # _houseId        .~ 4
                                       # _leadId         .~ 296285
                                       # _roofplates     .~ roofs
                                       # _panels         .~ panels
                                       # _dataServer     .~ serverUrl
                                       # _panelType      .~ panelType
                                       # _textureInfo    .~ textures
                                       # _rotBtnTexture  .~ rotateButtonPNG
                                       # _heatmapTexture .~ heatmapGradientPNG
                                       # _apiConfig      .~ apiCfg
                                       # _arrayEditParam .~ param

                        builderCfg = def # _leadId .~ 318872

                    editor <- createEditor el cfg

                    --house <- editHouse editor houseCfg

                    --void $ subscribe (house ^. _roofUpdate) logShow
                    --void $ subscribe (house ^. _serverUpdated) logShow
                    --void $ subscribe (house ^. _alignment) logShow
                    --void $ subscribe (house ^. _screenshot) logShow

                    buildHouse editor builderCfg
