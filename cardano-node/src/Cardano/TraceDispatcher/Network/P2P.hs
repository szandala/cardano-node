{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.TraceDispatcher.Network.P2P
  (
    namesForLocalRootPeers
  , severityLocalRootPeers
  , docLocalRootPeers

  , namesForPublicRootPeers
  , severityPublicRootPeers
  , docPublicRootPeers

  , namesForPeerSelection
  , severityPeerSelection
  , docPeerSelection

  , namesForDebugPeerSelection
  , severityDebugPeerSelection
  , docDebugPeerSelection

  , namesForPeerSelectionCounters
  , severityPeerSelectionCounters
  , docDebugSelectionCounters

  ) where

import           Cardano.Logging
import           Cardano.Prelude hiding (group, show)
import           Data.Aeson (ToJSONKey, Value (..), toJSON, toJSONList, (.=))
import qualified Data.Set as Set
import           Data.Text (pack)
import           Network.Socket (SockAddr)
import           Prelude (id, show)

import           Cardano.Node.Configuration.Topology ()
import           Cardano.Node.Configuration.TopologyP2P ()
import           Cardano.Tracing.OrphanInstances.Network ()

import qualified Ouroboros.Network.PeerSelection.EstablishedPeers as EstablishedPeers
import           Ouroboros.Network.PeerSelection.Governor
                     (DebugPeerSelection (..), PeerSelectionState (..),
                     PeerSelectionTargets (..), TracePeerSelection (..))
import qualified Ouroboros.Network.PeerSelection.KnownPeers as KnownPeers
import           Ouroboros.Network.PeerSelection.RelayAccessPoint
                     (RelayAccessPoint)
import           Ouroboros.Network.PeerSelection.RootPeersDNS
                     (TraceLocalRootPeers (..), TracePublicRootPeers (..))
import           Ouroboros.Network.PeerSelection.Types ()

namesForLocalRootPeers :: TraceLocalRootPeers ntnAddr resolverError -> [Text]
namesForLocalRootPeers TraceLocalRootDomains {} = ["LocalRootDomains"]
namesForLocalRootPeers TraceLocalRootWaiting {} = ["LocalRootWaiting"]
namesForLocalRootPeers TraceLocalRootResult {}  = ["LocalRootResult"]
namesForLocalRootPeers TraceLocalRootGroups {}  = ["LocalRootGroups"]
namesForLocalRootPeers TraceLocalRootFailure {} = ["LocalRootFailure"]
namesForLocalRootPeers TraceLocalRootError {}   = ["LocalRootError"]

severityLocalRootPeers :: TraceLocalRootPeers ntnAddr resolverError -> SeverityS
severityLocalRootPeers _ = Info

instance (ToJSONKey ntnAddr, ToJSONKey RelayAccessPoint, Show ntnAddr, Show exception) =>
    LogFormatting (TraceLocalRootPeers ntnAddr exception) where
  forMachine _dtal (TraceLocalRootDomains groups) =
    mkObject [ "kind" .= String "LocalRootDomains"
             , "localRootDomains" .= toJSON groups
             ]
  forMachine _dtal (TraceLocalRootWaiting d dt) =
    mkObject [ "kind" .= String "LocalRootWaiting"
             , "domainAddress" .= toJSON d
             , "diffTime" .= show dt
             ]
  forMachine _dtal (TraceLocalRootResult d res) =
    mkObject [ "kind" .= String "LocalRootResult"
             , "domainAddress" .= toJSON d
             , "result" .= toJSONList res
             ]
  forMachine _dtal (TraceLocalRootGroups groups) =
    mkObject [ "kind" .= String "LocalRootGroups"
             , "localRootGroups" .= toJSON groups
             ]
  forMachine _dtal (TraceLocalRootFailure d exception) =
    mkObject [ "kind" .= String "LocalRootFailure"
             , "domainAddress" .= toJSON d
             , "reason" .= show exception
             ]
  forMachine _dtal (TraceLocalRootError d exception) =
    mkObject [ "kind" .= String "LocalRootError"
             , "domainAddress" .= toJSON d
             , "reason" .= show exception
             ]
  forHuman = pack . show

docLocalRootPeers :: Documented (TraceLocalRootPeers ntnAddr resolverError)
docLocalRootPeers = Documented [
    DocMsg
      (TraceLocalRootDomains anyProto)
      []
      ""
  , DocMsg
      (TraceLocalRootWaiting anyProto anyProto)
      []
      ""
  , DocMsg
      (TraceLocalRootResult anyProto anyProto)
      []
      ""
  , DocMsg
      (TraceLocalRootGroups anyProto)
      []
      ""
  , DocMsg
      (TraceLocalRootFailure anyProto anyProto)
      []
      ""
  , DocMsg
      (TraceLocalRootError anyProto anyProto)
      []
      ""
  ]

-- data TracePublicRootPeers =
--        TracePublicRootRelayAccessPoint [RelayAccessPoint]
--      | TracePublicRootDomains [DomainAccessPoint]
--      | TracePublicRootResult  DNS.Domain [(IP, DNS.TTL)]
--      | TracePublicRootFailure DNS.Domain DNS.DNSError

namesForPublicRootPeers :: TracePublicRootPeers -> [Text]
namesForPublicRootPeers TracePublicRootRelayAccessPoint {} = ["PublicRootRelayAccessPoint"]
namesForPublicRootPeers TracePublicRootDomains {} = ["PublicRootDomains"]
namesForPublicRootPeers TracePublicRootResult {}  = ["PublicRootResult"]
namesForPublicRootPeers TracePublicRootFailure {}  = ["PublicRootFailure"]

severityPublicRootPeers :: TracePublicRootPeers -> SeverityS
severityPublicRootPeers _ = Info

instance LogFormatting TracePublicRootPeers where
  forMachine _dtal (TracePublicRootRelayAccessPoint relays) =
    mkObject [ "kind" .= String "PublicRootRelayAddresses"
             , "relayAddresses" .= toJSONList relays
             ]
  forMachine _dtal (TracePublicRootDomains domains) =
    mkObject [ "kind" .= String "PublicRootDomains"
             , "domainAddresses" .= toJSONList domains
             ]
  forMachine _dtal (TracePublicRootResult b res) =
    mkObject [ "kind" .= String "PublicRootResult"
             , "domain" .= show b
             , "result" .= toJSONList res
             ]
  forMachine _dtal (TracePublicRootFailure b d) =
    mkObject [ "kind" .= String "PublicRootFailure"
             , "domain" .= show b
             , "reason" .= show d
             ]
  forHuman = pack . show

docPublicRootPeers :: Documented TracePublicRootPeers
docPublicRootPeers = Documented [
    DocMsg
      (TracePublicRootRelayAccessPoint anyProto)
      []
      ""
  , DocMsg
      (TracePublicRootDomains anyProto)
      []
      ""
  , DocMsg
      (TracePublicRootResult anyProto anyProto)
      []
      ""
  , DocMsg
      (TracePublicRootFailure anyProto anyProto)
      []
      ""
  ]



namesForPeerSelection :: TracePeerSelection peeraddr -> [Text]
namesForPeerSelection TraceLocalRootPeersChanged {} = ["LocalRootPeersChanged"]
namesForPeerSelection TraceTargetsChanged {}        = ["TargetsChanged"]
namesForPeerSelection TracePublicRootsRequest {}    = ["ublicRootsRequest"]
namesForPeerSelection TracePublicRootsResults {}    = ["PublicRootsResults"]
namesForPeerSelection TracePublicRootsFailure {}    = ["PublicRootsFailure"]
namesForPeerSelection TraceGossipRequests {}        = ["GossipRequests"]
namesForPeerSelection TraceGossipResults {}         = ["GossipResults"]
namesForPeerSelection TraceForgetColdPeers {}       = ["ForgetColdPeers"]
namesForPeerSelection TracePromoteColdPeers {}      = ["PromoteColdPeers"]
namesForPeerSelection TracePromoteColdLocalPeers {} = ["PromoteColdLocalPeers"]
namesForPeerSelection TracePromoteColdFailed {}     = ["PromoteColdFailed"]
namesForPeerSelection TracePromoteColdDone {}       = ["PromoteColdDone"]
namesForPeerSelection TracePromoteWarmPeers {}      = ["PromoteWarmPeers"]
namesForPeerSelection TracePromoteWarmLocalPeers {} = ["PromoteWarmLocalPeers"]
namesForPeerSelection TracePromoteWarmFailed {}     = ["PromoteWarmFailed"]
namesForPeerSelection TracePromoteWarmDone {}       = ["PromoteWarmDone"]
namesForPeerSelection TraceDemoteWarmPeers {}       = ["DemoteWarmPeers"]
namesForPeerSelection TraceDemoteWarmFailed {}      = ["DemoteWarmFailed"]
namesForPeerSelection TraceDemoteWarmDone {}        = ["DemoteWarmDone"]
namesForPeerSelection TraceDemoteHotPeers {}        = ["DemoteHotPeers"]
namesForPeerSelection TraceDemoteLocalHotPeers {}   = ["DemoteLocalHotPeers"]
namesForPeerSelection TraceDemoteHotFailed {}       = ["DemoteHotFailed"]
namesForPeerSelection TraceDemoteHotDone {}         = ["DemoteHotDone"]
namesForPeerSelection TraceDemoteAsynchronous {}    = ["DemoteAsynchronous"]
namesForPeerSelection TraceGovernorWakeup {}        = ["GovernorWakeup"]
namesForPeerSelection TraceChurnWait {}             = ["ChurnWait"]
namesForPeerSelection TraceChurnMode {}             = ["ChurnMode"]


severityPeerSelection :: TracePeerSelection peeraddr -> SeverityS
severityPeerSelection TraceLocalRootPeersChanged {} = Notice
severityPeerSelection TraceTargetsChanged        {} = Notice
severityPeerSelection TracePublicRootsRequest    {} = Info
severityPeerSelection TracePublicRootsResults    {} = Info
severityPeerSelection TracePublicRootsFailure    {} = Error
severityPeerSelection TraceGossipRequests        {} = Debug
severityPeerSelection TraceGossipResults         {} = Debug
severityPeerSelection TraceForgetColdPeers       {} = Info
severityPeerSelection TracePromoteColdPeers      {} = Info
severityPeerSelection TracePromoteColdLocalPeers {} = Info
severityPeerSelection TracePromoteColdFailed     {} = Info
severityPeerSelection TracePromoteColdDone       {} = Info
severityPeerSelection TracePromoteWarmPeers      {} = Info
severityPeerSelection TracePromoteWarmLocalPeers {} = Info
severityPeerSelection TracePromoteWarmFailed     {} = Info
severityPeerSelection TracePromoteWarmDone       {} = Info
severityPeerSelection TraceDemoteWarmPeers       {} = Info
severityPeerSelection TraceDemoteWarmFailed      {} = Info
severityPeerSelection TraceDemoteWarmDone        {} = Info
severityPeerSelection TraceDemoteHotPeers        {} = Info
severityPeerSelection TraceDemoteLocalHotPeers   {} = Info
severityPeerSelection TraceDemoteHotFailed       {} = Info
severityPeerSelection TraceDemoteHotDone         {} = Info
severityPeerSelection TraceDemoteAsynchronous    {} = Info
severityPeerSelection TraceGovernorWakeup        {} = Info
severityPeerSelection TraceChurnWait             {} = Info
severityPeerSelection TraceChurnMode             {} = Info

instance LogFormatting (TracePeerSelection SockAddr) where
  forMachine _dtal (TraceLocalRootPeersChanged lrp lrp') =
    mkObject [ "kind" .= String "LocalRootPeersChanged"
             , "previous" .= toJSON lrp
             , "current" .= toJSON lrp'
             ]
  forMachine _dtal (TraceTargetsChanged pst pst') =
    mkObject [ "kind" .= String "TargetsChanged"
             , "previous" .= toJSON pst
             , "current" .= toJSON pst'
             ]
  forMachine _dtal (TracePublicRootsRequest tRootPeers nRootPeers) =
    mkObject [ "kind" .= String "PublicRootsRequest"
             , "targetNumberOfRootPeers" .= tRootPeers
             , "numberOfRootPeers" .= nRootPeers
             ]
  forMachine _dtal (TracePublicRootsResults res group dt) =
    mkObject [ "kind" .= String "PublicRootsResults"
             , "result" .= toJSONList (toList res)
             , "group" .= group
             , "diffTime" .= dt
             ]
  forMachine _dtal (TracePublicRootsFailure err group dt) =
    mkObject [ "kind" .= String "PublicRootsFailure"
             , "reason" .= show err
             , "group" .= group
             , "diffTime" .= dt
             ]
  forMachine _dtal (TraceGossipRequests targetKnown actualKnown aps sps) =
    mkObject [ "kind" .= String "GossipRequests"
             , "targetKnown" .= targetKnown
             , "actualKnown" .= actualKnown
             , "availablePeers" .= toJSONList (toList aps)
             , "selectedPeers" .= toJSONList (toList sps)
             ]
  forMachine _dtal (TraceGossipResults res) =
    mkObject [ "kind" .= String "GossipResults"
             , "result" .= toJSONList (map ( bimap show id <$> ) res)
             ]
  forMachine _dtal (TraceForgetColdPeers targetKnown actualKnown sp) =
    mkObject [ "kind" .= String "ForgeColdPeers"
             , "targetKnown" .= targetKnown
             , "actualKnown" .= actualKnown
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TracePromoteColdPeers targetKnown actualKnown sp) =
    mkObject [ "kind" .= String "PromoteColdPeers"
             , "targetEstablished" .= targetKnown
             , "actualEstablished" .= actualKnown
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TracePromoteColdLocalPeers tLocalEst aLocalEst sp) =
    mkObject [ "kind" .= String "PromoteColdLocalPeers"
             , "targetLocalEstablished" .= tLocalEst
             , "actualLocalEstablished" .= aLocalEst
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TracePromoteColdFailed tEst aEst p d err) =
    mkObject [ "kind" .= String "PromoteColdFailed"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             , "delay" .= toJSON d
             , "reason" .= show err
             ]
  forMachine _dtal (TracePromoteColdDone tEst aEst p) =
    mkObject [ "kind" .= String "PromoteColdDone"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             ]
  forMachine _dtal (TracePromoteWarmPeers tActive aActive sp) =
    mkObject [ "kind" .= String "PromoteWarmPeers"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TracePromoteWarmLocalPeers taa sp) =
    mkObject [ "kind" .= String "PromoteWarmLocalPeers"
             , "targetActualActive" .= toJSONList taa
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TracePromoteWarmFailed tActive aActive p err) =
    mkObject [ "kind" .= String "PromoteWarmFailed"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             , "reason" .= show err
             ]
  forMachine _dtal (TracePromoteWarmDone tActive aActive p) =
    mkObject [ "kind" .= String "PromoteWarmDone"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             ]
  forMachine _dtal (TraceDemoteWarmPeers tEst aEst sp) =
    mkObject [ "kind" .= String "DemoteWarmPeers"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TraceDemoteWarmFailed tEst aEst p err) =
    mkObject [ "kind" .= String "DemoteWarmFailed"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             , "reason" .= show err
             ]
  forMachine _dtal (TraceDemoteWarmDone tEst aEst p) =
    mkObject [ "kind" .= String "DemoteWarmDone"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             ]
  forMachine _dtal (TraceDemoteHotPeers tActive aActive sp) =
    mkObject [ "kind" .= String "DemoteHotPeers"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TraceDemoteLocalHotPeers taa sp) =
    mkObject [ "kind" .= String "DemoteLocalHotPeers"
             , "targetActualActive" .= toJSONList taa
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TraceDemoteHotFailed tActive aActive p err) =
    mkObject [ "kind" .= String "DemoteHotFailed"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             , "reason" .= show err
             ]
  forMachine _dtal (TraceDemoteHotDone tActive aActive p) =
    mkObject [ "kind" .= String "DemoteHotDone"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             ]
  forMachine _dtal (TraceDemoteAsynchronous msp) =
    mkObject [ "kind" .= String "DemoteAsynchronous"
             , "state" .= toJSON msp
             ]
  forMachine _dtal TraceGovernorWakeup =
    mkObject [ "kind" .= String "GovernorWakeup"
             ]
  forMachine _dtal (TraceChurnWait dt) =
    mkObject [ "kind" .= String "ChurnWait"
             , "diffTime" .= toJSON dt
             ]
  forMachine _dtal (TraceChurnMode c) =
    mkObject [ "kind" .= String "ChurnMode"
             , "event" .= show c ]

docPeerSelection :: Documented (TracePeerSelection SockAddr)
docPeerSelection = Documented [
    DocMsg
      (TraceLocalRootPeersChanged anyProto anyProto)
      []
      ""
  , DocMsg
      (TraceTargetsChanged anyProto anyProto)
      []
      ""
  , DocMsg
      (TracePublicRootsRequest 1 1)
      []
      ""
  , DocMsg
      (TracePublicRootsResults anyProto 1 anyProto)
      []
      ""
  , DocMsg
      (TracePublicRootsResults anyProto 1 anyProto)
      []
      ""
  , DocMsg
      (TraceGossipRequests 1 1 anyProto anyProto)
      []
      "target known peers, actual known peers, peers available for gossip,\
      \ peers selected for gossip"
  , DocMsg
      (TraceGossipResults [])
      []
      ""
  , DocMsg
      (TraceForgetColdPeers 1 1 anyProto)
      []
      "target known peers, actual known peers, selected peers"
  , DocMsg
      (TracePromoteColdPeers 1 1 anyProto)
      []
      "target established, actual established, selected peers"
  , DocMsg
      (TracePromoteColdLocalPeers 1 1 anyProto)
      []
      "target local established, actual local established, selected peers"
  , DocMsg
      (TracePromoteColdFailed 1 1 anyProto anyProto anyProto)
      []
      "target established, actual established, peer, delay until next\
      \ promotion, reason"
  , DocMsg
      (TracePromoteColdDone 1 1 anyProto)
      []
      "target active, actual active, selected peers"
  , DocMsg
      (TracePromoteWarmPeers 1 1 anyProto)
      []
      "target active, actual active, selected peers"
  , DocMsg
      (TracePromoteWarmLocalPeers [] anyProto)
      []
      "local per-group (target active, actual active), selected peers"
  , DocMsg
      (TracePromoteWarmFailed 1 1 anyProto anyProto)
      []
      "target active, actual active, peer, reason"
  , DocMsg
      (TracePromoteWarmDone 1 1 anyProto)
      []
      "target active, actual active, peer"
  , DocMsg
      (TraceDemoteWarmPeers 1 1 anyProto)
      []
      "target established, actual established, selected peers"
  , DocMsg
      (TraceDemoteWarmFailed 1 1 anyProto anyProto)
      []
      "target established, actual established, peer, reason"
  , DocMsg
      (TraceDemoteWarmDone 1 1 anyProto)
      []
      "target established, actual established, peer"
  , DocMsg
      (TraceDemoteHotPeers 1 1 anyProto)
      []
      "target active, actual active, selected peers"
  , DocMsg
      (TraceDemoteLocalHotPeers [] anyProto)
      []
      "local per-group (target active, actual active), selected peers"
  , DocMsg
      (TraceDemoteHotFailed 1 1 anyProto anyProto)
      []
      "target active, actual active, peer, reason"
  , DocMsg
      (TraceDemoteHotFailed 1 1 anyProto anyProto)
      []
      "target active, actual active, peer, reason"
  , DocMsg
      (TraceDemoteHotDone 1 1 anyProto )
      []
      "target active, actual active, peer"
  , DocMsg
      (TraceDemoteAsynchronous anyProto )
      []
      ""
  , DocMsg
      TraceGovernorWakeup
      []
      ""
  , DocMsg
      (TraceChurnWait anyProto)
      []
      ""
  , DocMsg
      (TraceChurnMode anyProto)
      []
      ""
  ]

peerSelectionTargetsToObject :: PeerSelectionTargets -> Value
peerSelectionTargetsToObject
  PeerSelectionTargets { targetNumberOfRootPeers,
                         targetNumberOfKnownPeers,
                         targetNumberOfEstablishedPeers,
                         targetNumberOfActivePeers } =
    Object $
      mkObject [ "roots" .= targetNumberOfRootPeers
               , "knownPeers" .= targetNumberOfKnownPeers
               , "established" .= targetNumberOfEstablishedPeers
               , "active" .= targetNumberOfActivePeers
               ]

namesForDebugPeerSelection :: DebugPeerSelection SockAddr peerConn -> [Text]
namesForDebugPeerSelection _ = ["GovernorState"]

severityDebugPeerSelection :: DebugPeerSelection SockAddr peerConn -> SeverityS
severityDebugPeerSelection _ = Debug

instance Show peerConn => LogFormatting (DebugPeerSelection SockAddr peerConn) where
  forMachine DNormal (TraceGovernorState blockedAt wakeupAfter
                   PeerSelectionState { targets, knownPeers, establishedPeers, activePeers }) =
    mkObject [ "kind" .= String "DebugPeerSelection"
             , "blockedAt" .= String (pack $ show blockedAt)
             , "wakeupAfter" .= String (pack $ show wakeupAfter)
             , "targets" .= peerSelectionTargetsToObject targets
             , "numberOfPeers" .=
                 Object (mkObject [ "known" .= KnownPeers.size knownPeers
                                  , "established" .= EstablishedPeers.size establishedPeers
                                  , "active" .= Set.size activePeers
                                  ])
             ]
  forMachine _ (TraceGovernorState blockedAt wakeupAfter ev) =
    mkObject [ "kind" .= String "DebugPeerSelection"
             , "blockedAt" .= String (pack $ show blockedAt)
             , "wakeupAfter" .= String (pack $ show wakeupAfter)
             , "peerSelectionState" .= String (pack $ show ev)
             ]

docDebugPeerSelection :: Documented (DebugPeerSelection SockAddr peerConn)
docDebugPeerSelection = Documented
  [  DocMsg
      (TraceGovernorState anyProto anyProto anyProto)
      []
      ""
  ]

-- Tracer m PeerSelectionCounters

-- data PeerSelectionCounter
