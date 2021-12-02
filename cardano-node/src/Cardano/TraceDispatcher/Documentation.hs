{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -Wno-unused-imports  #-}
{-# OPTIONS_GHC -Wno-deprecations  #-}


module Cardano.TraceDispatcher.Documentation
  ( docTracers
  ) where

import           Codec.CBOR.Read (DeserialiseFailure)
import           Data.Aeson.Types (ToJSON)
import           Data.Maybe (fromMaybe)
import qualified Data.Text.IO as T
import           Network.Mux (MuxTrace (..), WithMuxBearer (..))
import qualified Network.Socket as Socket

import           Cardano.Logging
import           Cardano.Logging.Resources
import           Cardano.Logging.Resources.Types
import           Cardano.Prelude hiding (trace)

import           Cardano.TraceDispatcher.Formatting ()
import           Cardano.TraceDispatcher.Tracers.BlockReplayProgress
import           Cardano.TraceDispatcher.Tracers.ChainDB
import           Cardano.TraceDispatcher.Tracers.Consensus
import           Cardano.TraceDispatcher.Tracers.Diffusion
import           Cardano.TraceDispatcher.Tracers.ForgingThreadStats
                     (ForgeThreadStats, docForgeStats, forgeThreadStats)
import           Cardano.TraceDispatcher.Tracers.KESInfo
import           Cardano.TraceDispatcher.Tracers.NodeToClient
import           Cardano.TraceDispatcher.Tracers.NodeToNode
import           Cardano.TraceDispatcher.Tracers.NonP2P
import           Cardano.TraceDispatcher.Tracers.P2P
import           Cardano.TraceDispatcher.Tracers.Peer
import           Cardano.TraceDispatcher.Tracers.Resources (namesForResources,
                     severityResources, startResourceTracer)
import           Cardano.TraceDispatcher.Tracers.Shutdown
import           Cardano.TraceDispatcher.Tracers.Startup
import qualified "trace-dispatcher" Control.Tracer as NT
import           Trace.Forward.Utils.DataPoint (DataPoint)

import           Cardano.Node.Configuration.Logging (EKGDirect)
import           Cardano.Node.Handlers.Shutdown (ShutdownTrace)
import           Cardano.Node.Queries (NodeKernelData)
import           Cardano.Node.Startup
import           Cardano.Node.TraceConstraints
import           Cardano.Node.Tracing

import qualified Cardano.BM.Data.Trace as Old
import           Cardano.Tracing.Config (TraceOptions (..))
import           Cardano.Tracing.OrphanInstances.Common (ToObject)
import           Cardano.Tracing.Tracers
import           "contra-tracer" Control.Tracer (Tracer (..))

import           Ouroboros.Consensus.Block (ConvertRawHash (..))
import           Ouroboros.Consensus.Block.Forging
import           Ouroboros.Consensus.BlockchainTime.WallClock.Util
                     (TraceBlockchainTimeEvent (..))
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Byron.Ledger.Config (BlockConfig)
import           Ouroboros.Consensus.Ledger.Inspect (LedgerEvent)
import           Ouroboros.Consensus.Ledger.Query (Query)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx,
                     GenTxId, TxId)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Mempool.API (TraceEventMempool (..))
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                     (TraceBlockFetchServerEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (TraceChainSyncClientEvent)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
                     (TraceChainSyncServerEvent)
import           Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
                     (TraceLocalTxSubmissionServerEvent (..))
import qualified Ouroboros.Consensus.Network.NodeToClient as NodeToClient
import qualified Ouroboros.Consensus.Network.NodeToClient as NtC
import qualified Ouroboros.Consensus.Network.NodeToNode as NodeToNode
import qualified Ouroboros.Consensus.Network.NodeToNode as NtN
import           Ouroboros.Consensus.Node (NetworkP2PMode (..))
import qualified Ouroboros.Consensus.Node.Run as Consensus
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import           Ouroboros.Consensus.Shelley.Ledger.Block
import qualified Ouroboros.Consensus.Shelley.Protocol.HotKey as HotKey
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB
import           Ouroboros.Consensus.Storage.Serialisation (SerialisedHeader)

import           Ouroboros.Network.Block (Point (..), Serialised, Tip)
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.BlockFetch.Decision
import           Ouroboros.Network.ConnectionId (ConnectionId)
import qualified Ouroboros.Network.Diffusion as Diffusion
import qualified Ouroboros.Network.Diffusion as ND
import qualified Ouroboros.Network.Diffusion.NonP2P as NonP2P
import qualified Ouroboros.Network.Diffusion.P2P as P2P
import           Ouroboros.Network.Driver.Simple (TraceSendRecv)
import           Ouroboros.Network.KeepAlive (TraceKeepAliveClient (..))
import           Ouroboros.Network.NodeToClient (LocalAddress,
                     NodeToClientVersion)
import           Ouroboros.Network.NodeToNode (ErrorPolicyTrace (..),
                     NodeToNodeVersion, RemoteAddress, WithAddr (..))
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (LocalStateQuery)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LTS
import           Ouroboros.Network.Protocol.TxSubmission.Type (TxSubmission)
import           Ouroboros.Network.Protocol.TxSubmission2.Type (TxSubmission2)
import           Ouroboros.Network.Snocket (LocalAddress (..))
import           Ouroboros.Network.Subscription.Dns (DnsTrace (..),
                     WithDomainName (..))
import           Ouroboros.Network.Subscription.Ip (WithIPList (..))
import           Ouroboros.Network.Subscription.Worker (SubscriptionTrace (..))
import           Ouroboros.Network.TxSubmission.Inbound
                     (TraceTxSubmissionInbound)
import           Ouroboros.Network.TxSubmission.Outbound
                     (TraceTxSubmissionOutbound)


-- Have to repeat the construction of the tracers here,
-- as the tracers are behind old tracer interface after construction in mkDispatchTracers.
-- Can be changed, when old tracers have gone
docTracers :: forall blk t peer remotePeer.
  ( Show t
  , forall result. Show (Query blk result)
  , TraceConstraints blk
  , LogFormatting (ChainDB.InvalidBlockReason blk)
  , LedgerSupportsProtocol blk
  , Consensus.RunNode blk
  , LogFormatting peer
  , LogFormatting remotePeer
  , Show remotePeer
  , Show (TxId (GenTx blk))
  , Show peer
  )
  => FilePath
  -> FilePath
  -> Proxy blk
  -> IO ()
docTracers configFileName outputFileName _ = do
    trConfig      <- readConfiguration configFileName
    let trBase    :: Trace IO FormattedMessage = docTracer (Stdout MachineFormat)
        trForward :: Trace IO FormattedMessage = docTracer Forwarder
        trDataPoint = docTracerDatapoint DatapointBackend
        mbTrEKG   :: Maybe (Trace IO FormattedMessage) = Just (docTracer EKGBackend)
    -- NodeInfo tracer
    nodeInfoTr <- mkDataPointTracer
                trDataPoint
                (const ["NodeInfo"])
    configureTracers trConfig docNodeInfoTraceEvent [nodeInfoTr]
    nodeInfoTrDoc <- documentTracer trConfig nodeInfoTr
      (docNodeInfoTraceEvent :: Documented NodeInfo)

    -- Resource tracer
    resourcesTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Resources"
                (const [])
                (const Info)
                allPublic
    configureTracers trConfig docResourceStats [resourcesTr]
    resourcesTrDoc <- documentTracer trConfig resourcesTr
      (docResourceStats :: Documented ResourceStats)

    -- Startup tracer
    startupTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Startup"
                namesStartupInfo
                (const Notice)
                allPublic
    configureTracers trConfig docStartupInfo [startupTr]
    startupTrDoc <- documentTracer trConfig startupTr
      (docStartupInfo :: Documented (StartupTrace blk))

    shutdownTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Shutdown"
                namesShutdown
                (const Warning)
                allPublic
    configureTracers trConfig docShutdown [shutdownTr]
    shutdownTrDoc <- documentTracer trConfig shutdownTr
      (docShutdown :: Documented ShutdownTrace)

    -- Peers tracer
    peersTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Peers"
                namesForPeers
                severityPeers
                allPublic
    configureTracers trConfig docPeers [peersTr]
    peersTrDoc <- documentTracer trConfig peersTr
      (docPeers :: Documented [PeerT blk])


    chainDBTr <- mkCardanoTracer'
                trBase trForward mbTrEKG
                "ChainDB"
                namesForChainDBTraceEvents
                severityChainDB
                allPublic
                withAddedToCurrentChainEmptyLimited
    configureTracers trConfig docChainDBTraceEvent [chainDBTr]
    chainDBTrDoc <- documentTracer trConfig chainDBTr
      (docChainDBTraceEvent :: Documented (ChainDB.TraceEvent blk))

    replayBlockTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ReplayBlock"
                namesForReplayBlockStats
                severityReplayBlockStats
                allPublic
    configureTracers trConfig docReplayedBlock [replayBlockTr]
    replayBlockTrDoc <- documentTracer trConfig replayBlockTr
      (docReplayedBlock :: Documented ReplayBlockStats)

---- Consensus tracers

    chainSyncClientTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncClient"
                namesForChainSyncClientEvent
                severityChainSyncClientEvent
                allPublic
    configureTracers trConfig docChainSyncClientEvent [chainSyncClientTr]
    chainSyncClientTrDoc <- documentTracer trConfig chainSyncClientTr
      (docChainSyncClientEvent :: Documented (BlockFetch.TraceLabelPeer
                                    (ConnectionId RemoteAddress)
                                    (TraceChainSyncClientEvent blk)))

    chainSyncServerHeaderTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncServerHeader"
                namesForChainSyncServerEvent
                severityChainSyncServerEvent
                allPublic
    configureTracers trConfig docChainSyncServerEvent [chainSyncServerHeaderTr]
    chainSyncServerHeaderTrDoc <- documentTracer trConfig chainSyncServerHeaderTr
      (docChainSyncServerEvent :: Documented (TraceChainSyncServerEvent blk))

    chainSyncServerBlockTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncServerBlock"
                namesForChainSyncServerEvent
                severityChainSyncServerEvent
                allPublic
    configureTracers trConfig docChainSyncServerEvent [chainSyncServerBlockTr]
    chainSyncServerBlockTrDoc <- documentTracer trConfig chainSyncServerBlockTr
      (docChainSyncServerEvent :: Documented (TraceChainSyncServerEvent blk))

    blockFetchDecisionTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchDecision"
                namesForBlockFetchDecision
                severityBlockFetchDecision
                allConfidential
    configureTracers trConfig docBlockFetchDecision [blockFetchDecisionTr]
    blockFetchDecisionTrDoc <- documentTracer trConfig blockFetchDecisionTr
      (docBlockFetchDecision :: Documented [BlockFetch.TraceLabelPeer
                                      remotePeer
                                      (FetchDecision [Point (Header blk)])])

    blockFetchClientTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchClient"
                namesForBlockFetchClient
                severityBlockFetchClient
                allPublic
    configureTracers trConfig docBlockFetchClient [blockFetchClientTr]
    blockFetchClientTrDoc <- documentTracer trConfig blockFetchClientTr
      (docBlockFetchClient :: Documented (BlockFetch.TraceLabelPeer
                                remotePeer
                                (BlockFetch.TraceFetchClientState (Header blk))))

    blockFetchServerTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchServer"
                namesForBlockFetchServer
                severityBlockFetchServer
                allPublic
    configureTracers trConfig docBlockFetchServer [blockFetchServerTr]
    blockFetchServerTrDoc <- documentTracer trConfig blockFetchServerTr
      (docBlockFetchServer :: Documented (TraceBlockFetchServerEvent blk))

    forgeKESInfoTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ForgeStateInfo"
                namesForKESInfo
                severityKESInfo
                allPublic
    configureTracers trConfig docForgeKESInfo [forgeKESInfoTr]
    forgeKESInfoTrDoc <- documentTracer trConfig forgeKESInfoTr
      (docForgeKESInfo :: Documented (Consensus.TraceLabelCreds HotKey.KESInfo))

    txInboundTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "TxInbound"
                namesForTxInbound
                severityTxInbound
                allPublic
    configureTracers trConfig docTxInbound [txInboundTr]
    txInboundTrDoc <- documentTracer trConfig txInboundTr
      (docTxInbound :: Documented (BlockFetch.TraceLabelPeer
                                    remotePeer
                                    (TraceTxSubmissionInbound txid tx)))

    txOutboundTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "TxOutbound"
                namesForTxOutbound
                severityTxOutbound
                allPublic
    configureTracers trConfig docTxOutbound [txOutboundTr]
    txOutboundTrDoc <- documentTracer trConfig txOutboundTr
      (docTxOutbound :: Documented (BlockFetch.TraceLabelPeer
                            remotePeer
                            (TraceTxSubmissionOutbound (TxId (GenTx blk)) tx)))

    localTxSubmissionServerTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "LocalTxSubmissionServer"
                namesForLocalTxSubmissionServer
                severityLocalTxSubmissionServer
                allPublic
    configureTracers trConfig docLocalTxSubmissionServer [localTxSubmissionServerTr]
    localTxSubmissionServerTrDoc <- documentTracer trConfig localTxSubmissionServerTr
      (docLocalTxSubmissionServer :: Documented (TraceLocalTxSubmissionServerEvent blk))


    mempoolTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Mempool"
                namesForMempool
                severityMempool
                allPublic
    configureTracers trConfig docMempool [mempoolTr]
    mempoolTrDoc <- documentTracer trConfig mempoolTr
      (docMempool :: Documented (TraceEventMempool blk))

    forgeTr    <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Forge"
                namesForForge
                severityForge
                allPublic

                -- TODO Tracers docforgeThreadStatsTr?
    forgeThreadStatsTr <- mkCardanoTracer'
                trBase trForward mbTrEKG
                "ForgeStats"
                namesForForge
                severityForge
                allPublic
                forgeThreadStats
    configureTracers trConfig docForge [forgeTr, forgeThreadStatsTr]
    forgeTrDoc <- documentTracer trConfig forgeTr
      (docForge :: Documented
        (Either (Consensus.TraceLabelCreds (Consensus.TraceForgeEvent blk))
                (Consensus.TraceLabelCreds TraceStartLeadershipCheckPlus)))

    blockchainTimeTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockchainTime"
                namesForBlockchainTime
                severityBlockchainTime
                allPublic
    configureTracers trConfig docBlockchainTime [blockchainTimeTr]
    blockchainTimeTrDoc <- documentTracer trConfig blockchainTimeTr
      (docBlockchainTime :: Documented (TraceBlockchainTimeEvent t))

-- Node to client

    keepAliveClientTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "KeepAliveClient"
                namesForKeepAliveClient
                severityKeepAliveClient
                allPublic
    configureTracers trConfig docKeepAliveClient [keepAliveClientTr]
    keepAliveClientTrDoc <- documentTracer trConfig keepAliveClientTr
      (docKeepAliveClient :: Documented (TraceKeepAliveClient peer))

    chainSyncTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncClient"
                namesForTChainSync
                severityTChainSync
                allPublic
    configureTracers trConfig docTChainSync [chainSyncTr]
    chainSyncTrDoc <- documentTracer trConfig chainSyncTr
      (docTChainSync :: Documented
        (BlockFetch.TraceLabelPeer peer (TraceSendRecv
          (ChainSync x (Point blk) (Tip blk)))))

    txSubmissionTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "TxSubmissionClient"
                namesForTTxSubmission
                severityTTxSubmission
                allPublic
    configureTracers trConfig docTTxSubmission [txSubmissionTr]
    txSubmissionTrDoc <- documentTracer trConfig txSubmissionTr
      (docTTxSubmission :: Documented
         (BlockFetch.TraceLabelPeer
            peer
            (TraceSendRecv
               (LTS.LocalTxSubmission
                  (GenTx blk) (ApplyTxErr blk)))))


    stateQueryTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "StateQueryClient"
                namesForTStateQuery
                severityTStateQuery
                allPublic
    configureTracers trConfig docTStateQuery [stateQueryTr]
    stateQueryTrDoc <- documentTracer trConfig stateQueryTr
      (docTStateQuery :: Documented
            (BlockFetch.TraceLabelPeer peer
             (TraceSendRecv
               (LocalStateQuery blk (Point blk) (Query blk)))))

-- Node to Node

    chainSyncNodeTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncNode"
                namesForTChainSyncNode
                severityTChainSyncNode
                allPublic
    configureTracers trConfig docTChainSync [chainSyncNodeTr]
    chainSyncNodeTrDoc <- documentTracer trConfig chainSyncNodeTr
      (docTChainSync :: Documented (BlockFetch.TraceLabelPeer peer (TraceSendRecv
          (ChainSync x (Point blk) (Tip blk)))))

    chainSyncSerialisedTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncSerialised"
                namesForTChainSyncSerialised
                severityTChainSyncSerialised
                allPublic
    configureTracers trConfig docTChainSync [chainSyncSerialisedTr]
    chainSyncSerialisedTrDoc <- documentTracer trConfig chainSyncSerialisedTr
      (docTChainSync :: Documented (BlockFetch.TraceLabelPeer peer (TraceSendRecv
          (ChainSync x (Point blk) (Tip blk)))))

    blockFetchTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetch"
                namesForTBlockFetch
                severityTBlockFetch
                allPublic
    configureTracers trConfig docTBlockFetch [blockFetchTr]
    blockFetchTrDoc <- documentTracer trConfig blockFetchTr
      (docTBlockFetch :: Documented
            (BlockFetch.TraceLabelPeer peer
             (TraceSendRecv
               (BlockFetch x (Point blk)))))

    blockFetchSerialisedTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchSerialised"
                namesForTBlockFetchSerialised
                severityTBlockFetchSerialised
                allPublic
    configureTracers trConfig docTBlockFetch [blockFetchSerialisedTr]
    blockFetchSerialisedTrDoc <- documentTracer trConfig blockFetchSerialisedTr
      (docTBlockFetch :: Documented
            (BlockFetch.TraceLabelPeer peer
             (TraceSendRecv
               (BlockFetch x (Point blk)))))

    txSubmissionNodeTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "TxSubmission"
                namesForTxSubmissionNode
                severityTxSubmissionNode
                allPublic
    configureTracers trConfig docTTxSubmissionNode [txSubmissionNodeTr]
    txSubmissionNodeTrDoc <- documentTracer trConfig txSubmissionNodeTr
      (docTTxSubmissionNode :: Documented
        (BlockFetch.TraceLabelPeer peer
          (TraceSendRecv
            (TxSubmission (GenTxId blk) (GenTx blk)))))

    txSubmission2Tr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "TxSubmission2"
                namesForTxSubmission2Node
                severityTxSubmission2Node
                allPublic
    configureTracers trConfig docTTxSubmission2Node [txSubmission2Tr]
    txSubmission2TrDoc <- documentTracer trConfig txSubmission2Tr
      (docTTxSubmission2Node :: Documented
        (BlockFetch.TraceLabelPeer peer
          (TraceSendRecv
            (TxSubmission2 (GenTxId blk) (GenTx blk)))))

-- -- Diffusion
--     dtMuxTr   <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "Mux"
--                 namesForMux
--                 severityMux
--                 allPublic
--     configureTracers trConfig docMux [dtMuxTr]
--     dtLocalMuxTr   <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "MuxLocal"
--                 namesForMux
--                 severityMux
--                 allPublic
--     configureTracers trConfig docMux [dtLocalMuxTr]
--     dtHandshakeTr   <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "Handshake"
--                 namesForHandshake
--                 severityHandshake
--                 allPublic
--     configureTracers trConfig docHandshake [dtHandshakeTr]
--     dtLocalHandshakeTr  <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "LocalHandshake"
--                 namesForLocalHandshake
--                 severityLocalHandshake
--                 allPublic
--     configureTracers trConfig docLocalHandshake [dtLocalHandshakeTr]
--     dtDiffusionInitializationTr   <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "DiffusionInit"
--                 namesForDiffusionInit
--                 severityDiffusionInit
--                 allPublic
--     configureTracers trConfig docDiffusionInit [dtDiffusionInitializationTr]
--     dtLedgerPeersTr   <- mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "LedgerPeers"
--                 namesForLedgerPeers
--                 severityLedgerPeers
--                 allPublic
--     configureTracers trConfig docLedgerPeers [dtLedgerPeersTr]
-- -- DiffusionTracersExtra
--     localRootPeersTr  <-  mkCardanoTracer
--       trBase trForward mbTrEKG
--       "LocalRootPeers"
--       namesForLocalRootPeers
--       severityLocalRootPeers
--       allPublic
--     configureTracers trConfig docLocalRootPeers [localRootPeersTr]
--     publicRootPeersTr  <-  mkCardanoTracer
--       trBase trForward mbTrEKG
--       "PublicRootPeers"
--       namesForPublicRootPeers
--       severityPublicRootPeers
--       allPublic
--     configureTracers trConfig docPublicRootPeers [publicRootPeersTr]
--     peerSelectionTr  <-  mkCardanoTracer
--       trBase trForward mbTrEKG
--       "PeerSelection"
--       namesForPeerSelection
--       severityPeerSelection
--       allPublic
--     configureTracers trConfig docPeerSelection [peerSelectionTr]
--     debugPeerSelectionTr  <-  mkCardanoTracer
--       trBase trForward mbTrEKG
--       "DebugPeerSelection"
--       namesForDebugPeerSelection
--       severityDebugPeerSelection
--       allPublic
--     configureTracers trConfig docDebugPeerSelection [debugPeerSelectionTr]
--     debugPeerSelectionResponderTr  <-  mkCardanoTracer
--       trBase trForward mbTrEKG
--       "DebugPeerSelectionResponder"
--       namesForDebugPeerSelection
--       severityDebugPeerSelection
--       allPublic
--     configureTracers trConfig docDebugPeerSelection [debugPeerSelectionResponderTr]
--     peerSelectionCountersTr  <-  mkCardanoTracer
--       trBase trForward mbTrEKG
--       "PeerSelectionCounters"
--       namesForPeerSelectionCounters
--       severityPeerSelectionCounters
--       allPublic
--     configureTracers trConfig docPeerSelectionCounters [peerSelectionCountersTr]
--     peerSelectionActionsTr  <-  mkCardanoTracer
--       trBase trForward mbTrEKG
--       "PeerSelectionActions"
--       namesForPeerSelectionActions
--       severityPeerSelectionActions
--       allPublic
--     configureTracers trConfig docPeerSelectionActions [peerSelectionActionsTr]
--     connectionManagerTr  <-  mkCardanoTracer
--       trBase trForward mbTrEKG
--       "ConnectionManager"
--       namesForConnectionManager
--       severityConnectionManager
--       allPublic
--     configureTracers trConfig docConnectionManager [connectionManagerTr]
--     serverTr  <-  mkCardanoTracer
--       trBase trForward mbTrEKG
--       "Server"
--       namesForServer
--       severityServer
--       allPublic
--     configureTracers trConfig docServer [serverTr]
--     inboundGovernorTr  <-  mkCardanoTracer
--       trBase trForward mbTrEKG
--       "InboundGovernor"
--       namesForInboundGovernor
--       severityInboundGovernor
--       allPublic
--     configureTracers trConfig docInboundGovernor [inboundGovernorTr]
--     localConnectionManagerTr  <-  mkCardanoTracer
--       trBase trForward mbTrEKG
--       "LocalConnectionManager"
--       namesForConnectionManager
--       severityConnectionManager
--       allPublic
--     configureTracers trConfig docConnectionManager [localConnectionManagerTr]
--     localServerTr  <-  mkCardanoTracer
--       trBase trForward mbTrEKG
--       "LocalServer"
--       namesForServer
--       severityServer
--       allPublic
--     configureTracers trConfig docServer [localServerTr]
--     localInboundGovernorTr  <-  mkCardanoTracer
--       trBase trForward mbTrEKG
--       "LocalInboundGovernor"
--       namesForInboundGovernor
--       severityInboundGovernor
--       allPublic
--     configureTracers trConfig docInboundGovernor [localInboundGovernorTr]
-- -- DiffusionTracersExtra nonP2P
--     dtIpSubscriptionTr   <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "IpSubscription"
--                 namesForIPSubscription
--                 severityIPSubscription
--                 allPublic
--     configureTracers trConfig docIPSubscription [dtIpSubscriptionTr]
--     dtDnsSubscriptionTr  <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "DNSSubscription"
--                 namesForDNSSubscription
--                 severityDNSSubscription
--                 allPublic
--     configureTracers trConfig docDNSSubscription [dtDnsSubscriptionTr]
--     dtDnsResolverTr  <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "DNSResolver"
--                 namesForDNSResolver
--                 severityDNSResolver
--                 allPublic
--     configureTracers trConfig docDNSResolver [dtDnsResolverTr]
--     dtErrorPolicyTr  <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "ErrorPolicy"
--                 namesForErrorPolicy
--                 severityErrorPolicy
--                 allPublic
--     configureTracers trConfig docErrorPolicy [dtErrorPolicyTr]
--     dtLocalErrorPolicyTr <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "LocalErrorPolicy"
--                 namesForLocalErrorPolicy
--                 severityLocalErrorPolicy
--                 allPublic
--     configureTracers trConfig docLocalErrorPolicy [dtLocalErrorPolicyTr]
--     dtAcceptPolicyTr    <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "AcceptPolicy"
--                 namesForAcceptPolicy
--                 severityAcceptPolicy
--                 allPublic
--     configureTracers trConfig docAcceptPolicy [dtAcceptPolicyTr]

    let bl =  nodeInfoTrDoc
            <> resourcesTrDoc
            <> startupTrDoc
            <> shutdownTrDoc
            <> peersTrDoc
            <> chainDBTrDoc
            <> replayBlockTrDoc
-- Consensus
            <> chainSyncClientTrDoc
            <> chainSyncServerHeaderTrDoc
            <> chainSyncServerBlockTrDoc
            <> blockFetchDecisionTrDoc
            <> blockFetchClientTrDoc
            <> blockFetchServerTrDoc
            <> forgeKESInfoTrDoc
            <> txInboundTrDoc
            <> txOutboundTrDoc
            <> localTxSubmissionServerTrDoc
            <> mempoolTrDoc
            <> forgeTrDoc
            <> blockchainTimeTrDoc
-- NodeToClient
            <> keepAliveClientTrDoc
            <> chainSyncTrDoc
            <> txSubmissionTrDoc
            <> stateQueryTrDoc
-- Node to Node
            <> chainSyncNodeTrDoc
            <> chainSyncSerialisedTrDoc
            <> blockFetchTrDoc
            <> blockFetchSerialisedTrDoc
            <> txSubmissionNodeTrDoc
            <> txSubmission2TrDoc
-- Diffusion


    res <- buildersToText bl trConfig
    T.writeFile outputFileName res
    pure ()

-- -- ChainDB
--     cdbmTrDoc <- documentTracer trConfig cdbmTr
--       (docChainDBTraceEvent :: Documented (ChainDB.TraceEvent blk))
--
-- -- Consensus
--     cscTrDoc <- documentTracer trConfig cscTr
--         (docChainSyncClientEvent  :: Documented
--           (BlockFetch.TraceLabelPeer Peer
--             (TraceChainSyncClientEvent blk)))
--     csshTrDoc <- documentTracer trConfig csshTr
--         (docChainSyncServerEvent  :: Documented
--           (TraceChainSyncServerEvent blk))
--     cssbTrDoc <- documentTracer trConfig cssbTr
--         (docChainSyncServerEvent  :: Documented
--           (TraceChainSyncServerEvent blk))
--     bfdTrDoc <- documentTracer trConfig bfdTr
--         (docBlockFetchDecision :: Documented
--            [BlockFetch.TraceLabelPeer Peer
--              (FetchDecision [Point (Header blk)])])
--     bfcTrDoc <- documentTracer trConfig bfcTr
--         (docBlockFetchClient :: Documented
--           (BlockFetch.TraceLabelPeer Peer
--             (BlockFetch.TraceFetchClientState (Header blk))))
--     bfsTrDoc <- documentTracer trConfig bfsTr
--         (docBlockFetchServer :: Documented
--           (TraceBlockFetchServerEvent blk))
--     fsiTrDoc <- documentTracer trConfig fsiTr
--         (docForgeStateInfo :: Documented
--            (Consensus.TraceLabelCreds HotKey.KESInfo))
--     txiTrDoc <- documentTracer trConfig txiTr
--         (docTxInbound :: Documented
--            (BlockFetch.TraceLabelPeer Peer
--              (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk))))
--     txoTrDoc <- documentTracer trConfig txoTr
--         (docTxOutbound :: Documented
--           (BlockFetch.TraceLabelPeer Peer
--             (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk))))
--     ltxsTrDoc <- documentTracer trConfig ltxsTr
--         (docLocalTxSubmissionServer :: Documented
--           (TraceLocalTxSubmissionServerEvent blk))
--     mpTrDoc <- documentTracer trConfig mpTr
--         (docMempool :: Documented
--           (TraceEventMempool blk))
--     fTrDoc <- documentTracer trConfig fTr
--         (docForge :: Documented
--           (ForgeTracerType blk))
--     -- fSttTrDoc <- documentTracer trConfig fSttTr TODO JNF
--     --     (docForgeStats :: Documented
--     --       ForgeThreadStats)
--     btTrDoc <- documentTracer trConfig btTr
--         (docBlockchainTime :: Documented
--           (TraceBlockchainTimeEvent t))
--     kacTrDoc <- documentTracer trConfig kacTr
--         (docKeepAliveClient :: Documented
--           (TraceKeepAliveClient Peer))
--
--     tcsTrDoc <- documentTracer trConfig tcsTr
--         (docTChainSync :: Documented
--           (BlockFetch.TraceLabelPeer Peer
--             (TraceSendRecv
--               (ChainSync (Serialised blk) (Point blk) (Tip blk)))))
--     ttsTrDoc <- documentTracer trConfig ttsTr
--         (docTTxSubmission :: Documented
--            (BlockFetch.TraceLabelPeer
--               Peer
--               (TraceSendRecv
--                  (LTS.LocalTxSubmission
--                     (GenTx blk) (ApplyTxErr blk)))))
--     tsqTrDoc <- documentTracer trConfig tsqTr
--         (docTStateQuery :: Documented
--            (BlockFetch.TraceLabelPeer Peer
--             (TraceSendRecv
--               (LocalStateQuery blk (Point blk) (Query blk)))))
--     tcsnTrDoc <- documentTracer trConfig tcsnTr
--         (docTChainSync :: Documented
--           (BlockFetch.TraceLabelPeer Peer
--             (TraceSendRecv
--               (ChainSync (Header blk) (Point blk) (Tip blk)))))
--     tcssTrDoc <- documentTracer trConfig tcssTr
--         (docTChainSync :: Documented
--           (BlockFetch.TraceLabelPeer Peer
--             (TraceSendRecv
--               (ChainSync (SerialisedHeader blk) (Point blk) (Tip blk)))))
--     tbfTrDoc <- documentTracer trConfig tbfTr
--         (docTBlockFetch :: Documented
--           (BlockFetch.TraceLabelPeer Peer
--             (TraceSendRecv
--               (BlockFetch blk (Point blk)))))
--     tbfsTrDoc <- documentTracer trConfig tbfsTr
--         (docTBlockFetch :: Documented
--           (BlockFetch.TraceLabelPeer Peer
--             (TraceSendRecv
--               (BlockFetch (Serialised blk) (Point blk)))))
--     tsnTrDoc <- documentTracer trConfig tsnTr
--         (docTTxSubmissionNode :: Documented
--           (BlockFetch.TraceLabelPeer Peer
--             (TraceSendRecv
--               (TxSubmission (GenTxId blk) (GenTx blk)))))
--     ts2nTrDoc <- documentTracer trConfig ts2nTr
--         (docTTxSubmission2Node :: Documented
--           (BlockFetch.TraceLabelPeer Peer
--             (TraceSendRecv
--               (TxSubmission2 (GenTxId blk) (GenTx blk)))))
--     ipsTrDoc <- documentTracer trConfig ipsTr
--         (docIPSubscription :: Documented
--           (WithIPList (SubscriptionTrace Socket.SockAddr)))
--     dnssTrDoc <- documentTracer trConfig dnssTr
--         (docDNSSubscription :: Documented
--           (WithDomainName (SubscriptionTrace Socket.SockAddr)))
--     dnsrTrDoc <- documentTracer trConfig dnsrTr
--         (docDNSResolver :: Documented (WithDomainName DnsTrace))
--     errpTrDoc <- documentTracer trConfig errpTr
--         (docErrorPolicy :: Documented
--           (WithAddr Socket.SockAddr ErrorPolicyTrace))
--     lerrpTrDoc <- documentTracer trConfig lerrpTr
--         (docLocalErrorPolicy :: Documented
--           (WithAddr LocalAddress ErrorPolicyTrace))
--     apTrDoc <- documentTracer trConfig apTr
--         (docAcceptPolicy :: Documented
--           NtN.AcceptConnectionsPolicyTrace)
--     muxTrDoc <- documentTracer trConfig muxTr
--         (docMux :: Documented
--           (WithMuxBearer Peer MuxTrace))
--     muxLTrDoc <- documentTracer trConfig muxLTr
--         (docMux :: Documented
--           (WithMuxBearer Peer MuxTrace))
--     hsTrDoc <- documentTracer trConfig hsTr
--         (docHandshake :: Documented NtN.HandshakeTr)
--     lhsTrDoc <- documentTracer trConfig lhsTr
--         (docLocalHandshake :: Documented NtC.HandshakeTr)
--     diTrDoc <- documentTracer trConfig diTr
--         (docDiffusionInit :: Documented ND.DiffusionInitializationTracer)
--
--     rsTrDoc <- documentTracer trConfig rsTr
--         (docResourceStats :: Documented ResourceStats)
--     biTrDoc <- documentTracer trConfig biTr
--         (docBasicInfo :: Documented BasicInfo)
--     pTrDoc <- documentTracer trConfig pTr
--         (docPeers :: Documented [PeerT blk])
--
--     let bl = niTrDoc
--             ++ cdbmTrDoc
--             ++ cscTrDoc
--             ++ csshTrDoc
--             ++ cssbTrDoc
--             ++ bfdTrDoc
--             ++ bfcTrDoc
--             ++ bfsTrDoc
--             ++ fsiTrDoc
--             ++ txiTrDoc
--             ++ txoTrDoc
--             ++ ltxsTrDoc
--             ++ mpTrDoc
--             ++ fTrDoc
--             -- ++ fSttTrDoc
--             ++ btTrDoc
--             ++ kacTrDoc
--
--             ++ tcsTrDoc
--             ++ ttsTrDoc
--             ++ tsqTrDoc
--             ++ tcsnTrDoc
--             ++ tcssTrDoc
--             ++ tbfTrDoc
--             ++ tbfsTrDoc
--             ++ tsnTrDoc
--             ++ ts2nTrDoc
--             ++ ipsTrDoc
--             ++ dnssTrDoc
--             ++ dnsrTrDoc
--             ++ errpTrDoc
--             ++ lerrpTrDoc
--             ++ apTrDoc
--             ++ muxTrDoc
--             ++ muxLTrDoc
--             ++ hsTrDoc
--             ++ lhsTrDoc
--             ++ diTrDoc
--
--             ++ rsTrDoc
--             ++ biTrDoc
--             ++ pTrDoc
--
--     res <- buildersToText bl trConfig
--     T.writeFile outputFileName res
--     pure ()
