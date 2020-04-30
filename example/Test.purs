module Test where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Editor.Editor (createEditor, loadHouse)
import Effect (Effect)
import Effect.Class.Console (logShow)
import FRP.Event (subscribe)
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
        Right roofs -> case elem of
            Nothing -> pure unit
            Just e -> do
                editor <- createEditor 800 600 e
                res <- loadHouse serverUrl 296285 roofs editor
                
                _ <- subscribe res logShow
                pure unit