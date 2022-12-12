{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-orphans     #-}

module Marconi.Index.MintBurn where

import Control.Lens ((&), (^.))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as Short
import Data.Coerce (coerce)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Word (Word64)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.ToField qualified as SQL

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C

import Cardano.Ledger.Alonzo.Data qualified as LA
import Cardano.Ledger.Alonzo.Scripts qualified as LA
import Cardano.Ledger.Alonzo.Tx qualified as LA
import Cardano.Ledger.Alonzo.TxWitness qualified as LA
import Cardano.Ledger.Babbage.Tx qualified as LB

import Ouroboros.Consensus.Shelley.Eras qualified as OEra

import Cardano.Ledger.Mary.Value qualified as LM

import RewindableIndex.Index.VSqlite qualified as RewindableIndex


-- * Event

data TxMintEvent = TxMintEvent
  { txMintEventSlot     :: C.SlotNo
  , txMintEventBlockNo  :: C.BlockNo
  , txMintEventTxAssets :: [(C.TxId, [MintAsset])]
  } deriving Show

data MintAsset = MintAsset
  { mintAssetPolicyId     :: C.PolicyId
  , mintAssetAssetName    :: C.AssetName
  , mintAssetQuantity     :: C.Quantity
  , mintAssetRedeemerIdx  :: Word64
  , mintAssetRedeemerData :: C.ScriptData
  } deriving Show

toUpdate :: C.BlockInMode C.CardanoMode -> TxMintEvent
toUpdate (C.BlockInMode (C.Block (C.BlockHeader slotNo _ blockNo) txs) _) = TxMintEvent slotNo blockNo $
  map txMints txs

txMints :: C.Tx era -> (C.TxId, [MintAsset])
txMints (C.Tx txb _) = (C.getTxId txb, txbMints txb)

txbMints :: C.TxBody era -> [MintAsset]
txbMints txb = case txb of
  C.ShelleyTxBody era shelleyTx _ _ _ _ -> case era of
    C.ShelleyBasedEraShelley -> []
    C.ShelleyBasedEraAllegra -> []
    C.ShelleyBasedEraMary -> []
    C.ShelleyBasedEraAlonzo -> do
      (policyId, assetName, quantity, index', redeemer) <- getPolicyData txb $ LA.mint shelleyTx
      pure $ MintAsset policyId assetName quantity index' redeemer
    C.ShelleyBasedEraBabbage -> do
      (policyId, assetName, quantity, index', redeemer) <- getPolicyData txb $ LB.mint shelleyTx
      pure $ MintAsset policyId assetName quantity index' redeemer
  _ -> [] -- ByronTxBody is not exported but as it's the only other data constructor then _ matches it.

-- * Helpers

txRedeemers :: C.TxBody era -> Map.Map LA.RdmrPtr (LA.Data (C.ShelleyLedgerEra era), LA.ExUnits)
txRedeemers (C.ShelleyTxBody _ _ _ txScriptData _ _) = case txScriptData of
  C.TxBodyScriptData _proof _datum redeemers -> LA.unRedeemers redeemers
  C.TxBodyNoScriptData                       -> mempty
txRedeemers _ = mempty

mintRedeemers :: C.TxBody era -> [(Word64, (LA.Data (C.ShelleyLedgerEra era), LA.ExUnits))]
mintRedeemers txb = txRedeemers txb
  & Map.toList
  & filter (\(LA.RdmrPtr tag _, _) -> tag == LA.Mint)
  & map (\(LA.RdmrPtr _ w, a) -> (w, a))

getMaryOtherAssets :: LM.Value c -> Map.Map (LM.PolicyID c) (Map.Map LM.AssetName Integer)
getMaryOtherAssets (LM.Value _ m) = m

getPolicyData :: C.TxBody era -> LM.Value OEra.StandardCrypto -> [(C.PolicyId, C.AssetName, C.Quantity, Word64, C.ScriptData)]
getPolicyData txb value = do
  let
    policyIdList = Map.toList $ getMaryOtherAssets value
    getPolicyId index' = policyIdList !! fromIntegral index'
  ((maryPolicyID, assets), index'', (redeemer, _)) <- map (\(index', data_) -> (getPolicyId index', index', data_)) $ mintRedeemers txb
  (assetName, quantity) :: (LM.AssetName, Integer) <- Map.toList assets
  pure $ (fromMaryPolicyID maryPolicyID, fromMaryAssetName assetName, C.Quantity quantity, index'', fromAlonzoData redeemer)

-- ** Copy-paste

fromMaryPolicyID :: LM.PolicyID OEra.StandardCrypto -> C.PolicyId
fromMaryPolicyID (LM.PolicyID sh) = C.PolicyId (C.fromShelleyScriptHash sh) -- file:/home/iog/src/cardano-node/cardano-api/src/Cardano/Api/Value.hs::293

fromMaryAssetName :: LM.AssetName -> C.AssetName
fromMaryAssetName (LM.AssetName n) = C.AssetName $ Short.fromShort n -- file:/home/iog/src/cardano-node/cardano-api/src/Cardano/Api/Value.hs::296

fromAlonzoData :: LA.Data ledgerera -> C.ScriptData
fromAlonzoData = C.fromPlutusData . LA.getPlutusData -- file:/home/iog/src/cardano-node/cardano-api/src/Cardano/Api/ScriptData.hs::147

-- * Sqlite

instance SQL.ToField C.SlotNo where
  toField f = SQL.toField (coerce f :: Word64)

instance SQL.ToField C.BlockNo where
  toField f = SQL.toField (coerce f :: Word64)

instance SQL.ToField C.TxId where
  toField = SQL.toField . C.serialiseToRawBytes

instance SQL.ToField C.PolicyId where
  toField = SQL.toField . C.serialiseToRawBytes

instance SQL.ToField C.AssetName where
  toField f = SQL.toField (coerce f :: BS.ByteString)

instance SQL.ToField C.Quantity where
  toField f = SQL.toField (coerce f :: Integer)

instance SQL.ToField C.ScriptData where
  toField = SQL.toField . C.serialiseToCBOR

toRows :: TxMintEvent -> [[SQL.SQLData]]
toRows e = do
  (txId, txMintAssets) <- txMintEventTxAssets e
  mintAsset <- txMintAssets
  pure
    [ SQL.toField $ txMintEventSlot e
    , SQL.toField $ txMintEventBlockNo e

    , SQL.toField txId

    , SQL.toField $ mintAssetPolicyId mintAsset
    , SQL.toField $ mintAssetAssetName mintAsset
    , SQL.toField $ mintAssetQuantity mintAsset
    , SQL.toField $ mintAssetRedeemerIdx mintAsset
    , SQL.toField $ mintAssetRedeemerData mintAsset
    ]

sqliteInit :: SQL.Connection -> IO ()
sqliteInit c = liftIO $ SQL.execute_ c
  " CREATE TABLE IF NOT EXISTS minting_policy_event_table \
  \ ( Slot INT NOT NULL, BlockNumber INT NOT NULL \
  \ , TxId BLOB NOT NULL \
  \ , PolicyId BLOB NOT NULL, AssetName TEXT NOT NULL, Quantity INT NOT NULL, RedeemerIdx INT NOT NULL, RedeemerData BLOB NOT NULL)"

sqliteInsert :: SQL.Connection -> [TxMintEvent] -> IO ()
sqliteInsert c es = SQL.executeMany c template $ toRows =<< es
  where
    template =
      "INSERT INTO minting_policy_event_table \
      \        (Slot, BlockNumber, TxId, PolicyId, AssetName, Quantity, RedeemerIdx, RedeemerData ) \
      \ VALUES (?   , ?          , ?   , ?       , ?        , ?       , ?          , ? )"

-- * Indexer

type Result = ()
type Query = ()
type MintBurnIndex = RewindableIndex.SqliteIndex TxMintEvent () Query Result

open :: FilePath -> Int -> IO MintBurnIndex
open dbPath k = do
  indexer <- fromJust <$> RewindableIndex.newBoxed query store onInsert k ((k + 1) * 2) dbPath
  sqliteInit $ indexer ^. RewindableIndex.handle
  pure indexer

  where
    store :: MintBurnIndex -> IO ()
    store indexer = do
      buffered <- RewindableIndex.getBuffer $ indexer ^. RewindableIndex.storage
      sqliteInsert (indexer^.RewindableIndex.handle) buffered

    query :: MintBurnIndex -> Query -> [TxMintEvent] -> IO Result
    query _ _ _ = pure ()

    onInsert :: MintBurnIndex -> TxMintEvent -> IO [()]
    onInsert _ _ = pure []
