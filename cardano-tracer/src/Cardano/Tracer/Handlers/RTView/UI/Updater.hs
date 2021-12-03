{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Tracer.Handlers.RTView.UI.Updater
  ( updateUI
  ) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (readTVar, readTVarIO)
import           Control.Monad (forM_, unless, when)
import qualified Data.Map.Strict as M
import           Data.Set (Set, (\\))
import qualified Data.Set as S
import           Data.Text (unpack)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.UI.HTML.NodePanel
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types

updateUI
  :: UI.Window
  -> ConnectedNodes
  -> DisplayedNodes
  -> DisplayedElements
  -> AcceptedMetrics
  -> SavedTraceObjects
  -> UI ()
updateUI window connectedNodes displayedNodes displayedElements _acceptedMetrics savedTO = do
  (connected, displayed) <- liftIO . atomically $
    (,) <$> readTVar connectedNodes
        <*> readTVar displayedNodes
  when (connected /= displayed) $ do
    let disconnected   = displayed \\ connected -- In 'displayed' but not in 'connected'.
        newlyConnected = connected \\ displayed -- In 'connected' but not in 'displayed'.
    deletePanelsForDisconnected window disconnected
    addPanelsForConnected window newlyConnected
    checkNoNodesState window connected
    liftIO $ do
      updateDisplayedNodes displayedNodes connected
      updateDisplayedElements displayedElements connected
  -- Check if we have to update elements on the page using received 'TraceObject's.
  checkReceivedTraceObjects window displayedElements savedTO

addPanelsForConnected, deletePanelsForDisconnected
  :: UI.Window -> Set NodeId -> UI ()
addPanelsForConnected window = mapM_ (addNodePanel window)
deletePanelsForDisconnected window disconnected =
  forM_ disconnected $ \(NodeId anId) -> findAndDo window anId UI.delete

checkNoNodesState :: UI.Window -> Set NodeId -> UI ()
checkNoNodesState window connected =
  if S.null connected
    then findAndShow window "no-nodes"
    else findAndHide window "no-nodes"

checkReceivedTraceObjects
  :: UI.Window
  -> DisplayedElements
  -> SavedTraceObjects
  -> UI ()
checkReceivedTraceObjects window displayedElements savedTO = do
  savedTraceObjects <- liftIO $ readTVarIO savedTO
  forM_ (M.toList savedTraceObjects) $ \(nodeId, savedForNode) ->
    forM_ (M.toList savedForNode) $ \(namespace, toValue) ->
      updateElementsIfNeeded window displayedElements nodeId namespace toValue

updateElementsIfNeeded
  :: UI.Window
  -> DisplayedElements
  -> NodeId
  -> Namespace
  -> TraceObjectTValue
  -> UI ()
updateElementsIfNeeded window displayedElements nodeId namespace toValue = do
  case namespace of
    "density" -> updateElement
    "slotNum" -> return ()
    "blockNum" -> return ()
    "slotInEpoch" -> return ()
    "epoch" -> return ()
    "forks" -> return ()
    "txsInMempool"  -> return ()
    "mempoolBytes"  -> return ()
    "txsProcessedNum"  -> return ()
    "blocksForgedNum"  -> return ()
    "nodeCannotForge"  -> return ()
    "nodeIsLeaderNum"  -> return ()
    "slotsMissedNum" -> return ()
    "operationalCertificateStartKESPeriod"  -> return ()
    "operationalCertificateExpiryKESPeriod"  -> return ()
    "currentKESPeriod"  -> return ()
    "remainingKESPeriods" -> return ()
    _ -> return ()
 where
  updateElement = do
    let elId = ""
        elValue = toValue
    liftIO (getDisplayedValue displayedElements nodeId elId) >>= \case
      Nothing ->
        -- There is no displayed value for this element yet.
        setAndSave elId elValue
      Just displayedValue ->
        -- There is a value that already displayed, check if it changed.
        unless (elValue == displayedValue) $
          setAndSave elId elValue
   where
     setAndSave elId elValue = do
      findAndSet (set text $ unpack elValue) window elId
      liftIO $ saveDisplayedValue displayedElements nodeId elId elValue
