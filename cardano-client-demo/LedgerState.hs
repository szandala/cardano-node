{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

import           Cardano.Api
import           Control.Monad (when)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Char (ord)
import           Data.Foldable (toList)
import qualified Data.Text as T (unpack)
import           Data.Word (Word64)
import           System.Environment (getArgs)

import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley
import qualified Shelley.Spec.Ledger.Rewards as L
import qualified Shelley.Spec.Ledger.TxBody as L
import qualified Shelley.Spec.Ledger.RewardUpdate as L
import           Cardano.Crypto.Hash.Class (Hash (UnsafeHash))
import qualified Cardano.Ledger.ShelleyMA.TxBody as MA
import qualified Shelley.Spec.Ledger.API as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Base16 as Base16
import           Data.Coerce (coerce)
import           Data.Either (fromRight)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Set (Set)

data State =
  State { lastCheckpoint    :: Word64
        , lastRewStartEpoch :: Word64
        , lastRewEndEpoch   :: Word64
        , lastEra           :: String
        }

startingState :: State
startingState =
  State
    { lastCheckpoint = 0
    , lastRewStartEpoch = 0
    , lastRewEndEpoch = 0
    , lastEra = "byron"
    }

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

getAtMostOne :: (Show a, Show b) => Map a b -> Maybe (a, b)
getAtMostOne m =
  case Map.toList m of
    []   -> Nothing
    [(a, b)] -> Just (a, b)
    _ -> error $ "getAtMostOne received more than one element: " <> show m

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
msg = printEvent config
  where
    printEvent :: EventConf -> Event c -> IO ()
    printEvent conf ev = when (check conf ev) $ putStrLn $ message ev

    check :: EventConf -> Event c -> Bool
    check conf (CheckPoint _)      = checkPoint  conf
    check conf (NewEra _ _ _)      = newEra      conf
    check conf (Deleg _ _)         = deleg       conf
    check conf (Withdrawal _ _ _)  = withdrawal  conf
    check conf (RewardStart _ _ _) = rewardStart conf
    check conf (RewardEnd _ _ _)   = rewardEnd   conf

    message :: Event c -> String
    message (CheckPoint s)       =
      "CHECKPOINT - slot " <> show s
    message (NewEra e s name)    =
      "NEW-ERA - epoch " <> show e <> ", slot " <> show s <> ", " <> name
    message (Deleg s cert)       =
      "DELEG - slot " <> show s <> ", " <> show cert
    message (Withdrawal s ra c)  = "WDRL - slot "                           <> show s <> " " <> show ra <> " " <> show c
    message (RewardStart e s sd) = "REWARD-START - epoch " <> show e <> ", slot " <> show s <> ", " <> show (getAtMostOne sd)
    message (RewardEnd e s rs)   = "REWARD-END   - epoch " <> show e <> ", slot " <> show s <> ", " <> show (getAtMostOne rs)

hexStrToSBS :: String -> SBS.ShortByteString
hexStrToSBS =
  SBS.toShort . (fromRight undefined) . Base16.decode . (BS.pack . map (fromIntegral . ord))

hexStrToHash :: forall a c. String -> Cardano.Crypto.Hash.Class.Hash a c
hexStrToHash = coerce . UnsafeHash . hexStrToSBS

-- $ cabal exec
--     ledger-state
--     configuration/cardano/shelley_qa-config.json state-node-shelley_qa/node.socket
--     8190331207ecedfcaf448340d8bb84354a0e8c285982a2d7f98bb234
--     500000
main :: IO ()
main = do
  -- Get socket path from CLI argument.
  configFilePath : socketPath : target : checkpointSize : _ <- getArgs
  let f = either (error . T.unpack . renderFoldBlocksError) id
  _state <- fmap f $ runExceptT $ foldBlocks
    configFilePath
    socketPath
    (Testnet $ NetworkMagic 3)
    True -- enable validation?
    startingState
    (\_env
      !ledgerState
      (BlockInMode
        (Block (BlockHeader (SlotNo slotNo) _blockHeaderHash (BlockNo _blockNoI)) transactions)
        _era)
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

        mapM_ (newDelegs target slotNo . getTxBody) transactions
        mapM_ (newWdrl target slotNo . getTxBody) transactions

        lc <- displayCheckpoint slotNo (lastCheckpoint state) cps

        case info of
          Just (ep, L.SJust (L.Complete ru), goSnap) -> do
            era <- newEra name ep slotNo (lastEra state)
            es <- rewardStartEvent (lastRewStartEpoch state) ep slotNo goSnap    target
            ee <- rewardEndEvent   (lastRewEndEpoch   state) ep slotNo (L.rs ru) target
            return $ State { lastCheckpoint = lc
                           , lastRewStartEpoch = es
                           , lastRewEndEpoch = ee
                           , lastEra = era
                           }
          _ -> return $ state {lastCheckpoint = lc}
    )

  return ()
  where
    displayCheckpoint slot lastCP checkpointSize =
      if slot - lastCP >= checkpointSize
        then (msg $ CheckPoint slot) >> return slot
        else return lastCP

    newEra name (EpochNo ep) slotNo lastEra =
      if name /= lastEra
        then msg (NewEra ep slotNo name) >> return name
        else return lastEra

    newDelegs :: String -> Word64 -> TxBody era -> IO ()
    newDelegs _ _ (ByronTxBody _                               ) = return ()
    newDelegs t s (ShelleyTxBody ShelleyBasedEraShelley txb _ _) = newDelegs' t s $ L._certs txb
    newDelegs t s (ShelleyTxBody ShelleyBasedEraAllegra txb _ _) = newDelegs' t s $ MA.certs' txb
    newDelegs t s (ShelleyTxBody ShelleyBasedEraMary    txb _ _) = newDelegs' t s $ MA.certs' txb

    certEvent s c = msg $ Deleg s c
    newDelegs' t s cs = mapM_ (certEvent s) $ filter (onlyTargetD t) (toList cs)

    newWdrl :: String -> Word64 -> TxBody era -> IO ()
    newWdrl _ _ (ByronTxBody _                               ) = return ()
    newWdrl t s (ShelleyTxBody ShelleyBasedEraShelley txb _ _) = newWdrl' t s $ L.unWdrl $ L._wdrls txb
    newWdrl t s (ShelleyTxBody ShelleyBasedEraAllegra txb _ _) = newWdrl' t s $ L.unWdrl $ MA.wdrls' txb
    newWdrl t s (ShelleyTxBody ShelleyBasedEraMary    txb _ _) = newWdrl' t s $ L.unWdrl $ MA.wdrls' txb

    wdrlEvent s (ra, c) = msg $ Withdrawal s ra c
    onlyTargetW t (ra, _) = isTarget t $ L.getRwdCred ra
    newWdrl' t s ws = mapM_ (wdrlEvent s) $ filter (onlyTargetW t) (Map.toList ws)

    isTarget t (L.KeyHashObj    (L.KeyHash    kh)) = (hexStrToHash t) == kh
    isTarget t (L.ScriptHashObj (L.ScriptHash sh)) = False

    inPoolCert t pool = owner || rewardAccount
     where
       owner = any (\(L.KeyHash kh) -> hexStrToHash t == kh) $ toList (L._poolOwners pool)
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

    epochEvent event epochLast epochCurrent =
      if epochLast < epochCurrent then msg event >> return epochCurrent else return epochLast

    filterTargetAsKey t = Map.filterWithKey (\k _ -> isTarget t k)
    rewardStartEvent el (EpochNo e) slot ss t =
      epochEvent (RewardStart e slot (filterTargetAsKey t ss)) el e
    rewardEndEvent el (EpochNo e) slot rs t =
      epochEvent (RewardEnd e slot (filterTargetAsKey t rs)) el e
