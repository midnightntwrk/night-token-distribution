module Midnight.GlacierDrop.Transactions.RedemptionSupplySubdivide where

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley qualified as C
import Control.Error.Util (note)
import Control.Monad (when)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import Data.Set qualified as S
import Data.Traversable (for)
import GHC.IsList (IsList (fromList))
import Midnight.GlacierDrop.Api qualified as S
import Midnight.GlacierDrop.Contrib.Cardano.Api (AUTxO (AUTxO), foldToUTxO)
import Midnight.GlacierDrop.RedemptionTree (Hash (..))
import Midnight.GlacierDrop.Scripts.RedemptionSupply.Types qualified as S
import Midnight.GlacierDrop.Scripts.ThawingSchedule qualified as TS
import Midnight.GlacierDrop.Transactions.Common (
  AddressScriptHashError,
  DeserializeDatumError,
  SingleTokenValue (..),
  adjustForMinUTxO,
  deserializeDatumInline,
  getAddressScriptHash,
  getTxBody,
  makeHashableScriptData,
  makeScriptWitness,
  parseSingleTokenValue,
  valueToTxOutValue,
 )
import PlutusLedgerApi.Common qualified as P
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteString)

data RedemptionSupplySubdivideArgs = RedemptionSupplySubdivideArgs
  { desiredShardDepth :: Integer
  , eraHistory :: C.EraHistory
  , feeUTxO :: AUTxO C.ConwayEra
  -- ^ The funding UTxO for the transaction fee also used as collateral
  , protocolParams :: C.LedgerProtocolParameters C.ConwayEra
  , redemptionSupplyScriptUTxO :: AUTxO C.ConwayEra
  -- ^ FIXME: Turn this into (Either Script (AUTxO C.ConwayEra)) for testing without publishing
  , redemptionSupplyUTxO :: AUTxO C.ConwayEra
  -- ^ The actual redemption tree UTxO which we divide
  , subdivisionDepth :: S.SubdivisionDepth
  , subtrees :: [(ByteString, Integer)]
  , systemStart :: C.SystemStart
  }

data RedemptionSupplySubdivideError
  = AddressScriptHashError AddressScriptHashError
  | DeserializeDatumError DeserializeDatumError
  | InvalidInputValue C.Value
  | TooAgressiveSubdivision
  | InvalidOutputTotal {totalStarAmount :: Integer, subtreesTotal :: Integer}
  | TxBodyErrorAutoBalance (C.TxBodyErrorAutoBalance C.ConwayEra)
  | WrongState (C.TxOutDatum C.CtxUTxO C.ConwayEra)
  | WrongSubtreeOrder
  deriving (Show)

buildRedemptionSupplySubdivideTransaction
  :: RedemptionSupplySubdivideArgs
  -> Either RedemptionSupplySubdivideError (C.TxBody C.ConwayEra)
buildRedemptionSupplySubdivideTransaction args = do
  let RedemptionSupplySubdivideArgs
        { desiredShardDepth
        , eraHistory
        , feeUTxO
        , protocolParams
        , redemptionSupplyScriptUTxO
        , redemptionSupplyUTxO
        , subdivisionDepth
        , subtrees
        , systemStart
        } = args
      AUTxO
        redemptionSupplyTxIn
        (C.TxOut redemptionSupplyAddress redemptionSupplyValue redemptionSupplyDatum _) = redemptionSupplyUTxO

  (genesisTimestamp, redemptionIncrementPeriod, treeDepth) <-
    first DeserializeDatumError (deserializeDatumInline redemptionSupplyDatum) >>= \case
      S.RedemptionSupplyDatum _ t r (S.Subdividing d) -> pure (t, r, d)
      _ -> Left (WrongState redemptionSupplyDatum)

  SingleTokenValue (C.Quantity totalLovelace) nightAssetId totalStarAmount <- do
    let value = C.txOutValueToValue redemptionSupplyValue
    note (InvalidInputValue value) $ parseSingleTokenValue value
  let subtreesTotal = sum (snd <$> subtrees)
      (subtreesNonEmpty, subtreesEmpty) = break ((== 0) . snd) $ first Hash <$> subtrees
  when (totalStarAmount /= subtreesTotal) $
    Left $
      InvalidOutputTotal{totalStarAmount, subtreesTotal}
  when (not $ all ((== 0) . snd) subtreesEmpty) $ Left $ WrongSubtreeOrder

  txOuts <- do
    let desiredShardDepth' = S.RedemptionShardDepth (S.MerkleTreeDepth desiredShardDepth)
    mkSubTreesOutputs
      subdivisionDepth
      treeDepth
      subtreesNonEmpty
      genesisTimestamp
      redemptionIncrementPeriod
      desiredShardDepth'
      nightAssetId
      redemptionSupplyAddress
      protocolParams
      (L.Coin totalLovelace)
  redemptionSupplyScriptHash <-
    first AddressScriptHashError $ getAddressScriptHash redemptionSupplyAddress
  let AUTxO feeTxIn (C.TxOut changeAddress _ _ _) = feeUTxO
      utxoSet =
        foldToUTxO
          [ redemptionSupplyScriptUTxO
          , redemptionSupplyUTxO
          , feeUTxO
          ]
      AUTxO redemptionSupplyScriptTxIn _ = redemptionSupplyScriptUTxO
      redeemer =
        S.RedemptionSupplySubdivide
          subdivisionDepth
          (fmap toPlutusMerkleHash $ fmap fst $ subtreesEmpty)
      txBodyContent =
        (C.defaultTxBodyContent C.ShelleyBasedEraConway)
          { C.txIns =
              [ (feeTxIn, C.BuildTxWith . C.KeyWitness $ C.KeyWitnessForSpending)
              ,
                ( redemptionSupplyTxIn
                , makeScriptWitness redemptionSupplyScriptTxIn redemptionSupplyScriptHash redeemer
                )
              ]
          , C.txInsCollateral = C.TxInsCollateral C.AlonzoEraOnwardsConway [feeTxIn]
          , C.txInsReference =
              C.TxInsReference
                C.BabbageEraOnwardsConway
                [redemptionSupplyScriptTxIn]
                (C.BuildTxWith S.empty)
          , C.txOuts = txOuts
          , C.txProtocolParams = C.BuildTxWith (Just protocolParams)
          , C.txValidityLowerBound = C.TxValidityNoLowerBound
          , C.txValidityUpperBound = C.TxValidityUpperBound C.ShelleyBasedEraConway Nothing
          }
  fmap getTxBody
    . first TxBodyErrorAutoBalance
    $ C.makeTransactionBodyAutoBalance
      C.ShelleyBasedEraConway
      systemStart
      (C.toLedgerEpochInfo eraHistory)
      protocolParams
      mempty
      mempty
      mempty
      utxoSet
      txBodyContent
      changeAddress
      Nothing

mkSubTreesOutputs
  :: S.SubdivisionDepth
  -> S.MerkleTreeDepth
  -> [(Hash, Integer)]
  -> TS.GenesisTimestamp
  -> TS.RedemptionIncrementPeriod
  -> S.RedemptionShardDepth
  -> C.AssetId
  -> C.AddressInEra C.ConwayEra
  -> C.LedgerProtocolParameters C.ConwayEra
  -> L.Coin
  -> Either RedemptionSupplySubdivideError [C.TxOut C.CtxTx C.ConwayEra]
mkSubTreesOutputs subdivisionDepth treeDepth subtrees genesisTimestamp redemptionIncrementPeriod (S.RedemptionShardDepth desiredShardDepth) nightAssetId redemptionSupplyAddress protocolParams inputLovelace = do
  let subtreeDepth = do
        let S.MerkleTreeDepth td = treeDepth
            S.SubdivisionDepth sd = subdivisionDepth
        S.MerkleTreeDepth $ td - sd
  mkSubTreeOutput <- do
    mkSubTreeDatum <- case compare subtreeDepth desiredShardDepth of
      LT -> Left TooAgressiveSubdivision
      EQ -> pure \hash ->
        -- we substract `3` here because of the bytestring since each byte is 8 bits (2^3)
        let maskSize = 2 ^ (S.getMerkleTreeDepth desiredShardDepth - 3)
         in S.RedemptionSupplyDatum
              (toPlutusMerkleHash hash)
              genesisTimestamp
              redemptionIncrementPeriod
              ( S.Materializing $
                  S.MaterializationMask $
                    stringToBuiltinByteString $
                      replicate maskSize '\NUL'
              )
      GT -> pure \hash ->
        S.RedemptionSupplyDatum
          (toPlutusMerkleHash hash)
          genesisTimestamp
          redemptionIncrementPeriod
          (S.Subdividing subtreeDepth)
    pure \(hash, starQuantity) -> do
      let starValue =
            fromList
              [
                ( nightAssetId
                , C.Quantity starQuantity
                )
              ]
          datum =
            C.TxOutDatumInline C.BabbageEraOnwardsConway
              . makeHashableScriptData
              . mkSubTreeDatum
              $ hash
      pure . adjustForMinUTxO id protocolParams $
        C.TxOut
          redemptionSupplyAddress
          (valueToTxOutValue starValue)
          datum
          C.ReferenceScriptNone

  outputs <- for subtrees $ \(hash, starQuantity) -> do
    mkSubTreeOutput (hash, starQuantity)

  let outputTotalLovelace = flip foldMap outputs $ \(C.TxOut _ value _ _) -> do
        let lovelace = C.selectLovelace $ C.txOutValueToValue value
        lovelace
      missingLovelace = max mempty (inputLovelace - outputTotalLovelace)
  -- Add missing lovelace to the first output
  case outputs of
    [] -> pure outputs
    (C.TxOut addr value datum ref) : rest -> do
      let outValue = C.txOutValueToValue value
          outValue' = valueToTxOutValue $ C.lovelaceToValue missingLovelace <> outValue
      pure $ C.TxOut addr outValue' datum ref : rest

toPlutusMerkleHash :: Hash -> S.MerkleHash
toPlutusMerkleHash (Hash h) = S.MerkleHash (P.toBuiltin h)
