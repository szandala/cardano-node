{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.Run
  ( runRTView
  , module Cardano.Tracer.Handlers.RTView.State.TraceObjects
  ) where

import           Control.Monad (void)
import           Control.Monad.Extra (whenJust)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (UI, on, set, (#), (#+))

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.UI.CSS.Bulma
import           Cardano.Tracer.Handlers.RTView.UI.CSS.Own
import           Cardano.Tracer.Handlers.RTView.UI.HTML.PageBody
import           Cardano.Tracer.Handlers.RTView.UI.Updater
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types (AcceptedMetrics, ConnectedNodes, DataPointAskers)

runRTView
  :: TracerConfig
  -> ConnectedNodes
  -> AcceptedMetrics
  -> DataPointAskers
  -> SavedTraceObjects
  -> IO ()
runRTView TracerConfig{hasRTView} connectedNodes acceptedMetrics dpAskers savedTO =
  whenJust hasRTView $ \(Endpoint host port) -> do
    -- Initialize displayed stuff outside of main page renderer, to be able to update
    -- corresponding elements after reloading of the page.
    displayedNodes <- initDisplayedNodes
    displayedElements <- initDisplayedElements
    -- Run the web-server.
    UI.startGUI (config host port) $
      mkMainPage connectedNodes displayedNodes displayedElements acceptedMetrics dpAskers savedTO
 where
  config h p = UI.defaultConfig
    { UI.jsPort = Just . fromIntegral $ p
    , UI.jsAddr = Just . encodeUtf8 . T.pack $ h
    }

mkMainPage
  :: ConnectedNodes
  -> DisplayedNodes
  -> DisplayedElements
  -> AcceptedMetrics
  -> DataPointAskers
  -> SavedTraceObjects
  -> UI.Window
  -> UI ()
mkMainPage connectedNodes displayedNodes displayedElements acceptedMetrics _dpAskers savedTO window = do
  void $ return window # set UI.title pageTitle
  void $ UI.getHead window #+
    [ -- UI.link # set UI.rel "icon" # set UI.href "..." -- TODO: add SVG favicon.
      UI.meta # set UI.name "viewport" # set UI.content "width=device-width, initial-scale=1"
    , UI.mkElement "style" # set UI.html bulmaCSS
    , UI.mkElement "style" # set UI.html bulmaTooltipCSS
    , UI.mkElement "style" # set UI.html ownCSS
    -- , UI.mkElement "script" # set UI.html chartJS
    ]

  pageBody <- mkPageBody window

  -- Prepare and run the timer, which will call 'updateUI' function every second.
  uiUpdateTimer <- UI.timer # set UI.interval 1000
  on UI.tick uiUpdateTimer . const $
    updateUI window connectedNodes displayedNodes displayedElements acceptedMetrics savedTO
  UI.start uiUpdateTimer

  on UI.disconnect window . const $
    -- The connection with the browser was dropped (probably user closed the tab),
    -- so 'uiUpdateTimer' should be stopped.
    UI.stop uiUpdateTimer

  void $ UI.element pageBody

{-

<link rel="icon" type="image/png" sizes="16x16" href="data:image/png;base64,
iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAAMFBMVEU0OkArMjhobHEoPUPFEBIu
O0L+AAC2FBZ2JyuNICOfGx7xAwTjCAlCNTvVDA1aLzQ3COjMAAAAVUlEQVQI12NgwAaCDSA0888G
CItjn0szWGBJTVoGSCjWs8TleQCQYV95evdxkFT8Kpe0PLDi5WfKd4LUsN5zS1sKFolt8bwAZrCa
GqNYJAgFDEpQAAAzmxafI4vZWwAAAABJRU5ErkJggg==" />

-}
