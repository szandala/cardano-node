
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}


module Cardano.PlutusExample.ScriptContextChecker where

import           Prelude hiding (($))

import           Cardano.Api
import           Cardano.Api.Byron
import           Cardano.Api.Shelley

import           Codec.Serialise
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (ExceptT (..))
import           Control.Monad.Trans.Except.Extra
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import           Data.Default (Default (def))
import           Data.Maybe
import           System.FilePath.Posix

import qualified Cardano.Ledger.Alonzo.TxInfo as Alonzo
import           Ledger hiding (singleton)
import           Ledger.TimeSlot
import qualified Ledger.Typed.Scripts as Scripts
import           Plutus.V1.Ledger.Credential
import           Plutus.V1.Ledger.DCert
import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified PlutusTx
import           PlutusTx.IsData.Class
import           PlutusTx.Prelude hiding (Semigroup (..), unless)
import qualified PlutusTx.Prelude as P

-- We make a custom redeemer type so that we can test that the
-- TestScriptContext is what we expect it to be and fail if not
-- NB: No way to test:
-- TxId. Why? The TxId is the hash of the txbody.
--   The txbody holds the redeemer so we can't calculate the txid and insert it
--   into the redeemer because that will change the txid
-- TxFee: Why? Is modified after trying to include it in the redeemer
data TestScriptContext
  = TestScriptContext
      -- Tx Fee
      [Ledger.TxOut]
     -- { tscInfoInputs      :: [TxInInfo] -- ^ Transaction inputs
     -- , tscInfoOutputs     :: [Ledger.TxOut] -- ^ Transaction outputs
     -- , tscInfoForge       :: Ledger.Value -- ^ The 'Value' minted by this transaction.
     -- , tscInfoDCert       :: [DCert] -- ^ Digests of certificates included in this transaction
     -- , tscInfoWdrl        :: [(StakingCredential, Integer)] -- ^ Withdrawals
     -- , tscInfoValidRange  :: POSIXTimeRange -- ^ The valid range for the transaction.
     -- , tscInfoSignatories :: [PubKeyHash] -- ^ Signatures provided with the transaction, attested that they all signed the tx
     -- , tscInfoData        :: [(DatumHash, Datum)]
     -- }

PlutusTx.unstableMakeIsData ''TestScriptContext

------------------------------------------------------

newtype MyCustomDatum = MyCustomDatum Integer
newtype MyCustomRedeemer = MyCustomRedeemer [Ledger.TxOut]

PlutusTx.unstableMakeIsData ''MyCustomDatum
PlutusTx.unstableMakeIsData ''MyCustomRedeemer

data ExampleTypedValidator
instance Scripts.ValidatorTypes ExampleTypedValidator where
    type instance DatumType ExampleTypedValidator    = MyCustomDatum
    type instance RedeemerType ExampleTypedValidator = MyCustomRedeemer

{-# INLINABLE mkValidator #-}
mkValidator :: MyCustomDatum-> MyCustomRedeemer -> ScriptContext -> Bool
mkValidator _datum (MyCustomRedeemer txouts) scriptContext =
  (numRedeemerOuts P.== numScriptContextOuts) P.&&
  (P.length (nub allOutputs) P.== numScriptContextOuts) P.&&
  (P.length (nub allOutputs) P.== numRedeemerOuts)
 where
   numRedeemerOuts = P.length txouts

   numScriptContextOuts = P.length $ Ledger.txInfoOutputs txInfo

   allOutputs = Ledger.txInfoOutputs txInfo P.++ txouts

   txInfo :: TxInfo
   txInfo = scriptContextTxInfo scriptContext

inst :: Scripts.TypedValidator ExampleTypedValidator
inst = Scripts.mkTypedValidator @ExampleTypedValidator
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @MyCustomDatum @MyCustomRedeemer

validator :: Plutus.Validator
validator = Scripts.validatorScript inst

script :: Plutus.Script
script = Plutus.unValidatorScript validator

scriptContextCheckAsShortBs :: SBS.ShortByteString
scriptContextCheckAsShortBs = SBS.toShort . LB.toStrict $ serialise script

scriptContextCheckScript :: PlutusScript PlutusScriptV1
scriptContextCheckScript = PlutusScriptSerialised scriptContextCheckAsShortBs

------------------------------------------------------

  {-P.all (P.== True)
    [ -- Tx ins
      -- TODO: Also check index of tx in
  --    P.map txInInfoResolved (tscInfoInputs testScriptContext) P.== P.map txInInfoResolved (txInfoInputs info)
      -- Tx outs
   --   tscInfoOutputs testScriptContext P.== txInfoOutputs info
  --  , tscInfoFee testScriptContext P.== txInfoFee info
  --    -- Minted values
  --  , tscInfoForge testScriptContext P.== txInfoForge info
  --    -- Certificates
  --    -- TODO: Fix
  --  -- , tscInfoDCert testScriptContext P.== txInfoDCert info
  --    -- Withdrawals
  --  , tscInfoWdrl testScriptContext P.== txInfoWdrl info
  --    -- Validity interval
  --  , tscInfoValidRange testScriptContext P.== txInfoValidRange info
  --    -- Required signatures
  --  , tscInfoSignatories testScriptContext P.== txInfoSignatories info
  --  , tscInfoData testScriptContext P.== txInfoData info
  --    -- TxId
  --  , tscInfoId testScriptContext P.== txInfoId info
    ]
 where
  info :: TxInfo
  info = scriptContextTxInfo sContext
-}



sampleTestScriptContextDataJSON :: LB.ByteString
sampleTestScriptContextDataJSON =
  Aeson.encode $ scriptDataToJson ScriptDataJsonDetailedSchema sampleTestScriptContextData

sampleTestScriptContextData :: ScriptData
sampleTestScriptContextData =
  fromPlutusData $ PlutusTx.builtinDataToData . toBuiltinData $ MyCustomRedeemer dummyTxOuts

ledgerVal :: Ledger.Value
ledgerVal = Alonzo.transValue $ toMaryValue $ lovelaceToValue 100

dummyTxOuts :: [Ledger.TxOut]
dummyTxOuts = []
  --  []
  --  []
  --  PlutusTx.Prelude.mempty
  --  PlutusTx.Prelude.mempty
  --  []
  --  []
  --  (slotToPOSIXTimeRange def $ Slot 42)
  --  []
  --  []
  --  (Ledger.TxId "dummy")

data ScriptContextError = NoScriptsInByronEra
                        | NoScriptsInEra
                        | ReadTxBodyError (FileError TextEnvelopeError)
                        deriving Show

txBodyToTestScriptContext :: TxBody era -> Either ScriptContextError TestScriptContext
txBodyToTestScriptContext (ByronTxBody _) = Left NoScriptsInByronEra
txBodyToTestScriptContext (ShelleyTxBody ShelleyBasedEraAlonzo txBody _scripts _scriptData _mAux _) =
  Right $ TestScriptContext dummyTxOuts
   -- []
   -- (catMaybes $ Prelude.map Alonzo.txInfoOut $ Prelude.map (toShelleyTxOut ShelleyBasedEraAlonzo) $ fromLedgerTxOuts ShelleyBasedEraAlonzo txBody)
   -- PlutusTx.Prelude.mempty
   -- PlutusTx.Prelude.mempty
   -- []
   -- []
   -- (slotToPOSIXTimeRange def $ Slot 42)
   -- []
   -- []
   -- (Ledger.TxId "dummy")
txBodyToTestScriptContext (ShelleyTxBody _ _ _ _ _ _) = Left NoScriptsInEra


testScriptContextToScriptData :: TestScriptContext -> ScriptData
testScriptContextToScriptData = fromPlutusData . PlutusTx.builtinDataToData . toBuiltinData

getScriptContextFromTxBody :: FilePath -> ExceptT ScriptContextError IO TestScriptContext
getScriptContextFromTxBody fp = do
  InAnyCardanoEra _era alonzoTxBody
    <- firstExceptT ReadTxBodyError
         . newExceptT
         $ readFileTextEnvelopeAnyOf
             [ FromSomeType (AsTxBody AsAlonzoEra) (InAnyCardanoEra AlonzoEra)
             ]
             fp
  hoistEither $ txBodyToTestScriptContext alonzoTxBody

txBodytoRedeemer :: FilePath -> ExceptT ScriptContextError IO ()
txBodytoRedeemer txbodyFp = do
  testScrContext <- getScriptContextFromTxBody txbodyFp
  let redeemer = testScriptContextToScriptData testScrContext
      outFp = dropFileName txbodyFp </> "script-context.redeemer"
  liftIO . LB.writeFile outFp $ Aeson.encode $ scriptDataToJson ScriptDataJsonDetailedSchema redeemer
