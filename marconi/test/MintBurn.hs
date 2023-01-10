{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
module MintBurn where

import Codec.Serialise (serialise)
import Control.Concurrent qualified as IO
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Either
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.String (fromString)
import Streaming.Prelude qualified as S

import Hedgehog (Gen, Property, assert, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Extras.Test qualified as HE
import Hedgehog.Extras.Test.Base qualified as H
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Streaming qualified as CS
import Gen.Cardano.Api.Typed qualified as CGen
import Plutus.Script.Utils.Typed as PSU
import Plutus.V1.Ledger.Api qualified as PlutusV1
import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusTx qualified
import PlutusTx.Builtins qualified as BI

import Marconi.Index.MintBurn qualified as MintBurn
import Marconi.Indexers qualified as Indexers
import RewindableIndex.Index.VSplit qualified as RI
import RewindableIndex.Storable qualified as RIStorable

import Helpers qualified

hot :: IO ()
hot = defaultMain $ testGroup "Hot" [testPropertyNamed "prop_2" "2" queryMintedValues]

tests :: TestTree
tests = testGroup "MintBurn"
  [ testPropertyNamed "prop_1" "1" mintsPreserved
  , testPropertyNamed "prop_2" "2" queryMintedValues
  -- , testPropertyNamed "prop_3" "3" queryMintedValues
  -- , testPropertyNamed "prop_4" "4" rewind
  -- , testPropertyNamed "prop_5" "5" testnet
  ]

-- | Create a transaction that mints a value, convertit to a @Tx@
-- through `makeTransactionBody` and `signShelleyTransaction` and find
-- that the minted assets are still there.
mintsPreserved :: Property
mintsPreserved = H.property $ do

  let
    policyScript :: PlutusV1.MintingPolicy
    -- policyScript = undefined
    policyScript = PlutusV1.mkMintingPolicyScript $$(PlutusTx.compile [||\_ _ -> ()||])

    policyAssets :: [(C.AssetName, C.Quantity)]
    policyAssets = [("someAsset", 123), ("otherAsset", 456)]

    (policyId, policyWitness, mintedValues) = mkMintValue policyScript policyAssets
    mintValue = C.TxMintValue C.MultiAssetInAlonzoEra mintedValues (C.BuildTxWith $ Map.singleton policyId policyWitness)

  C.Tx txb _ :: C.Tx C.AlonzoEra <- forAll (genTx mintValue) >>= \case
    Left err  -> fail $ "TxBodyError: " <> show err
    Right tx' -> return tx'

  -- Index the transaction:
  let
    mints = MintBurn.txbMints txb
    policyAssets' = map (\mint -> (MintBurn.mintAssetAssetName mint, MintBurn.mintAssetQuantity mint)) mints
    policyId' = map MintBurn.mintAssetPolicyId mints

  H.footnote $ "Assets to be created: " <> show policyAssets
  H.footnote $ "Assets gotten: " <> show policyAssets'

  -- The assets that were used to construct the transaction were found
  -- in the generate transaction:
  assert $ Set.fromList policyAssets == Set.fromList policyAssets'
    && Set.fromList policyId' == Set.singleton policyId

-- | Create transactions, index them, query indexer and find the events.
--
-- Apparently, as cardano-api doesn't exports only the view-only
-- pattern synonym Block from Cardano.Api.Block, and not ShelleyBlock,
-- then we can't construct a stream of C.BlockInMode and reuse the
-- worker from Marconi.Indexers :(.
queryMintedValues :: Property
queryMintedValues = H.property $ do
  tx1 <- H.leftFail =<< forAll (genTxMintN 1)
  tx2 <- H.leftFail =<< forAll (genTxMintN 2)
  tx3 <- H.leftFail =<< forAll (genTxMintN 3)
  MintBurn.MintBurnResult res <- liftIO $ do
    let index tx indexerMVar = do
          let event = MintBurn.MintBurnEvent $ MintBurn.TxMintEvent 0 0 [MintBurn.txMints tx]
          IO.modifyMVar_ indexerMVar (RIStorable.insert event)
    indexerMVar <- IO.newMVar =<< MintBurn.open ":memory:" 10
    index tx1 indexerMVar
    index tx2 indexerMVar
    index tx3 indexerMVar

    let doQuery indexer = RIStorable.query RIStorable.QEverything indexer (MintBurn.MintBurnQuery $ const True)
    indexer <- IO.readMVar indexerMVar
    doQuery indexer

  assert $ length res == 3

--  error "queryMintedValues"

rewind :: Property
rewind = undefined

testnet :: Property
testnet = undefined

-- * Helpers

mkMintValue
  :: PlutusV1.MintingPolicy -> [(C.AssetName, C.Quantity)]
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint C.AlonzoEra, C.Value)
mkMintValue policy policyAssets = (policyId, policyWitness, mintedValues)
  where
    serialisedPolicyScript :: C.PlutusScript C.PlutusScriptV1
    serialisedPolicyScript = C.PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ PlutusV1.unMintingPolicyScript policy

    policyId :: C.PolicyId
    policyId = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV1 serialisedPolicyScript :: C.PolicyId

    executionUnits :: C.ExecutionUnits
    executionUnits = C.ExecutionUnits {C.executionSteps = 1_000_000_000, C.executionMemory = 500_000 }
    redeemer :: C.ScriptData
    redeemer = C.fromPlutusData $ PlutusV1.toData ()
    policyWitness :: C.ScriptWitness C.WitCtxMint C.AlonzoEra
    policyWitness = C.PlutusScriptWitness C.PlutusScriptV1InAlonzo C.PlutusScriptV1
      (C.PScript serialisedPolicyScript) C.NoScriptDatumForMint redeemer executionUnits

    mintedValues :: C.Value
    mintedValues = C.valueFromList $ map (\(assetName, quantity) -> (C.AssetId policyId assetName, quantity)) policyAssets

genTx :: C.TxMintValue C.BuildTx C.AlonzoEra -> Gen (Either C.TxBodyError (C.Tx C.AlonzoEra))
genTx txMintValue = do
  txbc <- CGen.genTxBodyContent C.AlonzoEra
  txIn <- CGen.genTxIn
  pparams' :: C.ProtocolParameters <- CGen.genProtocolParameters
  let
    pparams = C.BuildTxWith $ Just pparams'
      { C.protocolParamUTxOCostPerWord = Just 1
      , C.protocolParamPrices = Just $ C.ExecutionUnitPrices 1 1
      , C.protocolParamMaxTxExUnits = Just $ C.ExecutionUnits 1 1
      , C.protocolParamMaxBlockExUnits = Just $ C.ExecutionUnits 1 1
      , C.protocolParamMaxValueSize = Just 1
      , C.protocolParamCollateralPercent = Just 1
      , C.protocolParamMaxCollateralInputs = Just 1
      }
    txbc' = txbc
      { C.txMintValue = txMintValue
      , C.txInsCollateral = C.TxInsCollateral C.CollateralInAlonzoEra [txIn]
      , C.txProtocolParams = pparams
      }
  pure $ do
    txb <- C.makeTransactionBody txbc'
    pure $ C.signShelleyTransaction txb []

-- | Generate tx with minting
genTxMintN :: Int -> Gen (Either C.TxBodyError (C.Tx C.AlonzoEra))
genTxMintN n = genTx mintValue
  where
    policyScript :: PlutusV1.MintingPolicy
    -- policyScript = undefined
    policyScript = PlutusV1.mkMintingPolicyScript $$(PlutusTx.compile [||\_ _ -> ()||])

    policyAssets :: [(C.AssetName, C.Quantity)]
    policyAssets = [(fromString $ "someAsset" <> show n, 123), (fromString $ "otherAsset" <> show n, 456)]

    (policyId, policyWitness, mintedValues) = mkMintValue policyScript policyAssets
    mintValue = C.TxMintValue C.MultiAssetInAlonzoEra mintedValues (C.BuildTxWith $ Map.singleton policyId policyWitness)
