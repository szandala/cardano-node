{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}


module Cardano.Tracing.Constraints
  ( TraceConstraints
  ) where

import           Prelude (Show)

import           Data.Aeson

import           Cardano.BM.Tracing (ToObject)
import           Cardano.Node.Queries (ConvertTxId, LedgerQueries)
import           Cardano.Logging (LogFormatting)
import           Cardano.Node.Queries.ConvertTxId (ConvertTxId)
import           Cardano.Node.Queries.KES (GetKESInfo (..), HasKESInfo (..),
                     HasKESMetricsData (..))
import           Cardano.Node.Queries.Ledger (LedgerQueries)

import           Cardano.Ledger.Alonzo (AlonzoEra)
import           Cardano.Ledger.Alonzo.PParams (PParamsUpdate)
import           Cardano.Ledger.Alonzo.Rules.Bbody (AlonzoBbodyPredFail)
import           Cardano.Ledger.Alonzo.Rules.Utxo (UtxoPredicateFailure)
import           Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoPredFail)
import           Cardano.Ledger.Alonzo.TxBody (TxOut)
import           Cardano.Ledger.Crypto (StandardCrypto)

import           Ouroboros.Consensus.Block (BlockProtocol, CannotForge,
                     ForgeStateUpdateError, Header, ConvertRawHash)
import           Ouroboros.Consensus.HeaderValidation (OtherHeaderEnvelopeError)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerError)
import           Ouroboros.Consensus.Ledger.Inspect (LedgerEvent)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr,
                     HasTxId, HasTxs (..))
import           Ouroboros.Consensus.Protocol.Abstract (ValidationErr)
import           Ouroboros.Consensus.Shelley.Ledger.Mempool (GenTx, TxId)

import           Ouroboros.Network.ConnectionId (ConnectionId)
import           Ouroboros.Network.Snocket (LocalAddress (..))
import           Ouroboros.Network.NodeToNode (RemoteAddress)

-- | Tracing-related constraints for monitoring purposes.
type TraceConstraints blk =
    ( ConvertTxId blk
    , HasTxs blk
    , HasTxId (GenTx blk)
    , LedgerQueries blk
    , ToJSON   (TxId (GenTx blk))
    , ToJSON   (TxOut (AlonzoEra StandardCrypto))
    , ToJSON   (PParamsUpdate (AlonzoEra StandardCrypto))
    , HasKESMetricsData blk
    , HasKESInfo blk

    , ToObject (ApplyTxErr blk)
    , ToObject (GenTx blk)
    , ToObject (Header blk)
    , ToObject (LedgerError blk)
    , ToObject (LedgerEvent blk)
    , ToObject (OtherHeaderEnvelopeError blk)
    , ToObject (ValidationErr (BlockProtocol blk))
    , ToObject (CannotForge blk)
    , ToObject (ForgeStateUpdateError blk)

    -- TODO: handle the implications in the new logging
    , ToObject (UtxoPredicateFailure (AlonzoEra StandardCrypto))
    , ToObject (AlonzoBbodyPredFail (AlonzoEra StandardCrypto))
    , ToObject (AlonzoPredFail (AlonzoEra StandardCrypto))

--    , LogFormatting (LedgerUpdate blk)
--    , LogFormatting (LedgerWarning blk)
--    , LogFormatting (LedgerEvent blk)
    , ConvertRawHash blk
    , GetKESInfo blk
    , LogFormatting (ApplyTxErr blk)
    , LogFormatting (GenTx blk)
    , LogFormatting (Header blk)
    , LogFormatting (LedgerError blk)
    , LogFormatting (OtherHeaderEnvelopeError blk)
    , LogFormatting (ValidationErr (BlockProtocol blk))
    , LogFormatting (CannotForge blk)
    , LogFormatting (ForgeStateUpdateError blk)
--    , LogFormatting (ConnectionId RemoteAddress)
--    , Show (ConnectionId RemoteAddress)

    , LogFormatting (UtxoPredicateFailure (AlonzoEra StandardCrypto))
    , LogFormatting (AlonzoBbodyPredFail (AlonzoEra StandardCrypto))
    , LogFormatting (AlonzoPredFail (AlonzoEra StandardCrypto))

--    , LogFormatting (LedgerEvent (HardForkBlock '[ByronBlock]))
--    , LogFormatting (LedgerEvent (HardForkBlock '[ShelleyBlock StandardShelley]))

    , Show blk
    , Show (Header blk)
    )
