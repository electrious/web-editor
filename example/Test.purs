module Test where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Default (def)
import Data.Either (Either(..))
import Data.Lens ((.~))
import Data.Tuple (fst)
import Editor.Common.Lenses (_leadId)
import Editor.Editor (createEditor)
import Editor.EditorMode (EditorMode(..))
import Editor.SceneEvent (size)
import Editor.WebEditor (_dataServer, _elem, _modeEvt, _roofPlates, _sizeEvt, runWebEditor)
import Effect (Effect)
import Effect.Class.Console (logShow)
import FRP.Event (subscribe)
import FRP.Event.Extra (after)
import Foreign (Foreign)
import Foreign.Generic (decode)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

serverUrl :: String
serverUrl = "https://s3.eu-west-1.amazonaws.com/data.electrious.com"

doTest :: Foreign -> Effect Unit
doTest roofDat = do
    w <- window
    doc <- document w
    elem <- getElementById "editor" (toNonElementParentNode doc)

    case runExcept $ decode roofDat of
        Left e -> logShow e
        Right roofs -> do
                let modeEvt = const Showing <$> after 10
                    sizeEvt = const (size 800 600) <$> after 2
                    
                    cfg = def # _elem       .~ elem
                              # _leadId     .~ 296285
                              # _roofPlates .~ roofs
                              # _dataServer .~ serverUrl
                              # _modeEvt    .~ modeEvt
                              # _sizeEvt    .~ sizeEvt
                res <- runWebEditor cfg createEditor
                
                _ <- subscribe (fst res) logShow

                pure unit