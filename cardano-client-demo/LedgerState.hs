{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

import           Cardano.Api
import           Control.Monad (when)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Foldable (toList)
import qualified Data.Text as T (unpack)
import           Data.Word (Word64)
import           System.Environment (getArgs)

import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley
import qualified Shelley.Spec.Ledger.Rewards as L
import qualified Shelley.Spec.Ledger.TxBody as L
import qualified Shelley.Spec.Ledger.RewardUpdate as L
import qualified Cardano.Ledger.ShelleyMA.TxBody as MA
import qualified Shelley.Spec.Ledger.API as L
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Set (Set)

data State = State { lastCheckpoint    :: Word64
                   , lastRewStartEpoch :: Word64
                   , lastRewEndEpoch   :: Word64
                   , lastEra           :: String
                   }

startingState :: State
startingState = State {lastCheckpoint = 0, lastRewStartEpoch = 0, lastRewEndEpoch = 0, lastEra = "byron"}

data Event c
  = CheckPoint Word64
  | NewEra Word64 Word64 String
  | Deleg Word64 (L.DCert c)
  | Withdrawal Word64 (L.RewardAcnt c) L.Coin
  | RewardStart Word64 Word64 (Map (L.Credential 'L.Staking c) L.Coin)
  | RewardEnd Word64 Word64 (Map (L.Credential 'L.Staking c) (Set (L.Reward c)))

data EventConf =
  EventConf
    { checkPoint  :: Bool
    , newEra      :: Bool
    , deleg       :: Bool
    , withdrawal  :: Bool
    , rewardStart :: Bool
    , rewardEnd   :: Bool
    }

check :: EventConf -> Event c -> Bool
check conf (CheckPoint _)      = checkPoint  conf
check conf (NewEra _ _ _)      = newEra      conf
check conf (Deleg _ _)         = deleg       conf
check conf (Withdrawal _ _ _)  = withdrawal  conf
check conf (RewardStart _ _ _) = rewardStart conf
check conf (RewardEnd _ _ _)   = rewardEnd   conf

p :: EventConf -> Event c -> IO ()
p conf ev = when (check conf ev) $ putStrLn $ message ev

dispSingletonMap :: (Show a, Show b) => Map a b -> String
dispSingletonMap m =
  case Map.toList m of
    []   -> "none"
    (a, b):_ -> show a <> " " <> show b

message :: Event c -> String
message (CheckPoint s)       = "CHECKPOINT" <> " "                      <> show s
message (NewEra e s name)    = "NEW-ERA"    <> " "     <> show e <> " " <> show s <> " " <> name
message (Deleg s cert)       = "DELEG - slot "                          <> show s <> " " <> show cert
message (Withdrawal s ra c)  = "WDRL - slot "                           <> show s <> " " <> show ra <> " " <> show c
message (RewardStart e s sd) = "REWARD-START - epoch " <> show e <> ", slot " <> show s <> ", " <> dispSingletonMap sd
message (RewardEnd e s rs)   = "REWARD-END   - epoch " <> show e <> ", slot " <> show s <> ", " <> dispSingletonMap rs

config :: EventConf
config =
  EventConf
    { checkPoint  = False
    , newEra      = True
    , deleg       = True
    , withdrawal  = True
    , rewardStart = True
    , rewardEnd   = True
    }

msg :: Event c -> IO ()
msg = p config

-- $ cabal exec ledger-state configuration/cardano/shelley_qa-config.json state-node-shelley_qa/node.socket 8190331207ecedfcaf448340d8bb84354a0e8c285982a2d7f98bb234 500000
main :: IO ()
main = do
  -- Get socket path from CLI argument.
  configFilePath : socketPath : target : checkpointSize : _ <- getArgs
  lastCheckpoint <- fmap (either (error . T.unpack . renderFoldBlocksError) id) $ runExceptT $ foldBlocks
    configFilePath
    socketPath
    (Testnet $ NetworkMagic 3)
    True -- enable validation?
    startingState
    (\_env
      !ledgerState
      (BlockInMode (Block (BlockHeader (SlotNo slotNo) _blockHeaderHash (BlockNo _blockNoI)) transactions) _era)
      state -> do
        let go = L.unStake . L._stake . L._pstakeGo . L.esSnapshots . L.nesEs
        let cps = read checkpointSize
        let (name, info) =
              case ledgerState of
                LedgerStateByron _                                     ->
                  ("byron",   Nothing)
                LedgerStateShelley (Shelley.ShelleyLedgerState _ ls _) ->
                  ("shelley", Just (L.nesEL ls, L.nesRu ls, go ls))
                LedgerStateAllegra (Shelley.ShelleyLedgerState _ ls _) ->
                  ("allegra", Just (L.nesEL ls, L.nesRu ls, go ls))
                LedgerStateMary    (Shelley.ShelleyLedgerState _ ls _) ->
                  ("mary",    Just (L.nesEL ls, L.nesRu ls, go ls))

        mapM_ (displayDeleg target slotNo . getTxBody) transactions
        mapM_ (displayWdrl target slotNo . getTxBody) transactions

        let lc = if newCheckpoint slotNo (lastCheckpoint state) cps then slotNo else (lastCheckpoint state)
        case info of
          Just (ep, L.SJust (L.Complete ru), goSnap) -> do
            era <- displayNewEra name ep slotNo (lastEra state)
            es <- dispRewardStart (lastRewStartEpoch state) ep slotNo goSnap    target
            ee <- dispRewardEnd   (lastRewEndEpoch   state) ep slotNo (L.rs ru) target
            return $ State {lastCheckpoint = lc, lastRewStartEpoch = es, lastRewEndEpoch = ee, lastEra = era}
          _ -> return $ state {lastCheckpoint = lc}
    )

  return ()
  where
    newCheckpoint s lc cps = s - lc >= cps
    displayCheckpoint era s lc cps = when (newCheckpoint s lc cps) (msg $ CheckPoint s)

    displayNewEra name (EpochNo ep) slotNo lastEra =
      if name /= lastEra
        then msg (NewEra ep slotNo name) >> return name
        else return lastEra

    displayDeleg :: String -> Word64 -> TxBody era -> IO ()
    displayDeleg _ _ (ByronTxBody _                               ) = return ()
    displayDeleg t s (ShelleyTxBody ShelleyBasedEraShelley txb _ _) = displayFilteredDs t s $ L._certs txb
    displayDeleg t s (ShelleyTxBody ShelleyBasedEraAllegra txb _ _) = displayFilteredDs t s $ MA.certs' txb
    displayDeleg t s (ShelleyTxBody ShelleyBasedEraMary    txb _ _) = displayFilteredDs t s $ MA.certs' txb

    dispCert s c = msg $ Deleg s c
    displayFilteredDs t s cs = mapM_ (dispCert s) $ filter (onlyTargetD t) (toList cs)

    displayWdrl :: String -> Word64 -> TxBody era -> IO ()
    displayWdrl _ _ (ByronTxBody _                               ) = return ()
    displayWdrl t s (ShelleyTxBody ShelleyBasedEraShelley txb _ _) = displayFilteredWs t s $ L.unWdrl $ L._wdrls txb
    displayWdrl t s (ShelleyTxBody ShelleyBasedEraAllegra txb _ _) = displayFilteredWs t s $ L.unWdrl $ MA.wdrls' txb
    displayWdrl t s (ShelleyTxBody ShelleyBasedEraMary    txb _ _) = displayFilteredWs t s $ L.unWdrl $ MA.wdrls' txb

    dispWdrl s (ra, c) = msg $ Withdrawal s ra c
    onlyTargetW t (ra, _) = isTarget t $ L.getRwdCred ra
    displayFilteredWs t s ws = mapM_ (dispWdrl s) $ filter (onlyTargetW t) (Map.toList ws)

    dispHash h = (tail . init . show) h
    isTarget t (L.KeyHashObj    (L.KeyHash    kh)) = dispHash kh == t
    isTarget t (L.ScriptHashObj (L.ScriptHash sh)) = dispHash sh == t

    inPoolCert t pool = owner || rewardAccount
     where
       owner = any (\kh -> dispHash kh == t) $ toList (L._poolOwners pool)
       rewardAccount = isTarget t $ (L.getRwdCred . L._poolRAcnt) pool

    inMir t (L.MIRCert _pot (L.StakeAddressesMIR m)) = any (isTarget t) $ Map.keys m
    inMir t _ = False

    onlyTargetD t (L.DCertDeleg   (L.RegKey cred))                    = isTarget t cred
    onlyTargetD t (L.DCertDeleg   (L.DeRegKey cred))                  = isTarget t cred
    onlyTargetD t (L.DCertDeleg   (L.Delegate (L.Delegation cred _))) = isTarget t cred
    onlyTargetD t (L.DCertPool    (L.RegPool p))                      = inPoolCert t p
    onlyTargetD t (L.DCertPool    (L.RetirePool _ _))                 = False
    onlyTargetD t (L.DCertGenesis _)                                  = False
    onlyTargetD t (L.DCertMir     (mir))                              = inMir t mir

    dispEpochEvent event el e = if el < e then msg event >> return e else return el

    filtTargetKey t = Map.filterWithKey (\k _ -> isTarget t k)
    dispRewardStart el (EpochNo e) slot ss t =
      dispEpochEvent (RewardStart e slot (filtTargetKey t ss)) el e
    dispRewardEnd el (EpochNo e) slot rs t =
      dispEpochEvent (RewardEnd e slot (filtTargetKey t rs)) el e
