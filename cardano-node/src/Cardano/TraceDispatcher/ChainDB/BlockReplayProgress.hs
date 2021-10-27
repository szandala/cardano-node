{-# LANGUAGE RecordWildCards #-}

module Cardano.TraceDispatcher.ChainDB.BlockReplayProgress
  ( severityReplayBlockStats
  , namesForReplayBlockStats
  , withReplayedBlock
  , docReplayedBlock
  , ReplayBlockStats(..)
  ) where

import           Data.Aeson (Value (String), (.=))
import           Data.Text (pack)

import           Cardano.Logging
import           Cardano.Prelude

import           Ouroboros.Consensus.Block (realPointSlot)
import           Ouroboros.Network.Block (pointSlot, unSlotNo)
import           Ouroboros.Network.Point (withOrigin)

import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB

data ReplayBlockStats = ReplayBlockStats
  { rpsCount    :: Int
  , rpsProgress :: Double
  }

emptyReplayBlockStats :: ReplayBlockStats
emptyReplayBlockStats = ReplayBlockStats 0 0.0

instance LogFormatting ReplayBlockStats where
  forMachine _dtal ReplayBlockStats {..} =
    mkObject
      [ "kind" .= String "ReplayBlockStats"
      , "progress" .= String (pack $ show rpsProgress)
      ]
  forHuman ReplayBlockStats {..} = "Block replay progress " <> show rpsProgress <> "%"
  asMetrics ReplayBlockStats {..} =
     [DoubleM "Block replay progress (%)" rpsProgress]

severityReplayBlockStats :: ReplayBlockStats -> SeverityS
severityReplayBlockStats _ = Info

namesForReplayBlockStats :: ReplayBlockStats -> Namespace
namesForReplayBlockStats _ = ["LedgerReplay"]

docReplayedBlock :: Documented ReplayBlockStats
docReplayedBlock = Documented [
    DocMsg
      emptyReplayBlockStats
      [("Block replay progress (%)",
        "Progress in percent")]
      "Counts up the percent of a block replay."
  ]

withReplayedBlock :: Trace IO ReplayBlockStats
    -> IO (Trace IO (ChainDB.TraceEvent blk))
withReplayedBlock tr =
    let tr' = filterTrace filterFunction tr
        tr'' = contramap unfold tr'
    in foldMTraceM replayBlockStats emptyReplayBlockStats tr''
  where
    filterFunction(_, Just _, _) = True
    filterFunction(_, Nothing, ReplayBlockStats {..}) =
      rpsCount `mod` 1000 == 0

replayBlockStats :: MonadIO m
  => ReplayBlockStats
  -> LoggingContext
  -> Maybe TraceControl
  -> ChainDB.TraceEvent blk
  -> m ReplayBlockStats
replayBlockStats ReplayBlockStats {..} _context _mbCtrl
    (ChainDB.TraceLedgerReplayEvent (LedgerDB.ReplayedBlock pt [] replayTo)) = do
      let slotno = toInteger $ unSlotNo (realPointSlot pt)
          endslot = toInteger $ withOrigin 0 unSlotNo (pointSlot replayTo)
          progress' = (fromInteger slotno * 100.0) / fromInteger (max slotno endslot)
      pure (ReplayBlockStats (rpsCount + 1) progress')
replayBlockStats st@ReplayBlockStats {} _context _mbCtrl _ = pure st
