{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Midnight.GlacierDrop.Transactions.RedemptionSupplyFull where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Lens qualified as L
import Control.Monad (when)
import Control.Monad.Except (MonadError, throwError)
import Convex.BuildTx
import Convex.CardanoApi.Lenses qualified as L
import Convex.PlutusLedger.V1 qualified as PL
import Convex.Scripts (
  fromHashableScriptData,
  toHashableScriptData,
 )
import Data.Bifunctor qualified
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.List (findIndex)
import Data.Time.Clock.POSIX
import GHC.Exts (IsList (..))
import Midnight.GlacierDrop.Api (MerkleHash (..))
import Midnight.GlacierDrop.Scripts.Common (newNightTokenName)
import Midnight.GlacierDrop.Scripts.Redemption.Types qualified as SR
import Midnight.GlacierDrop.Scripts.RedemptionSupply.Terms qualified as RSTTerms
import Midnight.GlacierDrop.Scripts.RedemptionSupply.Types qualified as RST
import Midnight.GlacierDrop.Scripts.ThawingSchedule qualified as TS
import Midnight.GlacierDrop.Transactions.Common (DeserializeDatumError (..))
import PlutusLedgerApi.Common qualified as P
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteString)

data SubdivideRedemptionError era
  = SubdivideRedemptionDeserializationError DeserializeDatumError
  | SubdivideRedemptionInvalidInputValue C.Value
  | SubdivideRedemptionTooAgressiveSubdivision
  | SubdivideRedemptionInvalidOutputTotal
      {totalStarAmount :: Integer, subtreesTotal :: Integer}
  | SubdivideRedemptionWrongState (C.TxOutDatum C.CtxUTxO era)
  | SubdivideRedemptionWrongSubtreeOrder
  deriving (Show)

redemptionSubdivideOutput
  :: ( MonadBuildTx era m
     , C.IsBabbageBasedEra era
     , C.HasScriptLanguageInEra C.PlutusScriptV3 era
     , MonadError (SubdivideRedemptionError era) m
     )
  => C.NetworkId
  -> C.LedgerProtocolParameters era
  -> RST.SubdivisionDepth
  -> RST.MerkleTreeDepth
  -> TS.GenesisTimestamp
  -> TS.RedemptionIncrementPeriod
  -> Integer
  -- ^ desired shard depth
  -> C.AssetId
  -> C.AddressInEra era
  -> (ByteString, Integer)
  -> m ()
redemptionSubdivideOutput
  _networkId
  ledgerProtocolParameters
  (RST.SubdivisionDepth subdivisionDepth)
  (RST.MerkleTreeDepth treeDepth)
  genesisTimestamp
  redemptionIncrementPeriod
  desiredShardDepth
  nightAssetId
  redemptionSupplyAddress
  (hash, starQuantity) =
    do
      let subtreeDepth = treeDepth - subdivisionDepth
      subtreeDatum <-
        C.TxOutDatumInline C.babbageBasedEra . toHashableScriptData <$> case compare subtreeDepth desiredShardDepth of
          LT -> throwError SubdivideRedemptionTooAgressiveSubdivision
          EQ ->
            pure $
              let maskSize = 2 ^ (desiredShardDepth - 3)
               in RST.RedemptionSupplyDatum
                    (toPlutusMerkleHash hash)
                    genesisTimestamp
                    redemptionIncrementPeriod
                    ( RST.Materializing $
                        RST.MaterializationMask $
                          stringToBuiltinByteString $
                            replicate maskSize '\NUL'
                    )
          GT ->
            pure $
              RST.RedemptionSupplyDatum
                (toPlutusMerkleHash hash)
                genesisTimestamp
                redemptionIncrementPeriod
                (RST.Subdividing $ RST.MerkleTreeDepth subtreeDepth)

      let redemptionShardValue = fromList [(nightAssetId, C.Quantity starQuantity)]
          redemptionShardOutput =
            setMinAdaDeposit ledgerProtocolParameters $
              C.TxOut
                redemptionSupplyAddress
                (mkTxOutValue redemptionShardValue)
                subtreeDatum
                C.ReferenceScriptNone

      addOutput redemptionShardOutput

subdivideRedemptionTx
  :: ( MonadBuildTx era m
     , C.IsBabbageBasedEra era
     , C.HasScriptLanguageInEra C.PlutusScriptV3 era
     , MonadError (SubdivideRedemptionError era) m
     )
  => C.NetworkId
  -> C.LedgerProtocolParameters era
  -> C.TxIn
  -> C.TxOut C.CtxUTxO era
  -> C.PlutusScript C.PlutusScriptV3
  -> Maybe C.TxIn
  -> C.PlutusScript C.PlutusScriptV3
  -> Integer
  -> RST.SubdivisionDepth
  -> [(ByteString, Integer)]
  -> m ()
subdivideRedemptionTx
  networkId
  ledgerProtocolParameters
  redemptionSupplyTxIn
  redemptionSupplyInput
  redemptionSupplyScript
  redemptionSupplyRefTxIn
  nightMintingScript
  desiredShardDepth
  subdivisionDepth
  subtrees =
    do
      ( RST.RedemptionSupplyDatum
          _rootHash
          genesisTimestamp
          redemptionIncrementPeriod
          state
        ) <-
        maybe
          (throwError (SubdivideRedemptionDeserializationError MissingInputDatum))
          ( maybe
              (throwError (SubdivideRedemptionDeserializationError InvalidInputDatum))
              pure
              . fromHashableScriptData @RST.RedemptionSupplyDatum
          )
          (L.preview (L._TxOut . L._3 . L._TxOutDatumInline) redemptionSupplyInput)

      let redemptionSupplyAddress = L.view (L._TxOut . L._1) redemptionSupplyInput

      treeDepth <- case state of
        RST.Subdividing d -> pure d
        RST.Materializing _ ->
          throwError
            (SubdivideRedemptionWrongState $ L.view (L._TxOut . L._3) redemptionSupplyInput)

      let redemptionSupplyValue = L.view (L._TxOut . L._2 . L._TxOutValue) redemptionSupplyInput
          nightPolicyId =
            C.PolicyId $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 nightMintingScript
          nightAssetId = C.AssetId nightPolicyId (PL.unTransAssetName newNightTokenName)
          (C.Quantity totalStarAmount) = C.selectAsset redemptionSupplyValue nightAssetId
          (C.Quantity totalLovelaceFromRedemptionScript) = C.selectAsset redemptionSupplyValue C.AdaAssetId

      let subtreesTotal = sum (snd <$> subtrees)
          (subtreesNonEmpty, subtreesEmpty) = span ((> 0) . snd) subtrees

      when (any ((/= 0) . snd) subtreesEmpty) $
        throwError SubdivideRedemptionWrongSubtreeOrder

      when (totalStarAmount /= subtreesTotal) $
        throwError
          SubdivideRedemptionInvalidOutputTotal{totalStarAmount, subtreesTotal}

      traverse_
        ( redemptionSubdivideOutput
            networkId
            ledgerProtocolParameters
            subdivisionDepth
            treeDepth
            genesisTimestamp
            redemptionIncrementPeriod
            desiredShardDepth
            nightAssetId
            redemptionSupplyAddress
        )
        subtreesNonEmpty

      addBtx $ \txBody ->
        let (C.Quantity totalLovelaceToRedemptionScript) =
              sum
                $ map
                  ( \txo -> C.selectAsset (L.view (L._TxOut . L._2 . L._TxOutValue) txo) C.AdaAssetId
                  )
                $ filter
                  (\txo -> L.view (L._TxOut . L._1) txo == redemptionSupplyAddress)
                  (L.view L.txOuts txBody)
         in case findIndex
              (\txo -> L.view (L._TxOut . L._1) txo == redemptionSupplyAddress)
              (L.view L.txOuts txBody) of
              Just firstRedemptionOutputIdx ->
                if totalLovelaceToRedemptionScript < totalLovelaceFromRedemptionScript
                  then
                    L.over
                      (L.txOuts . L.ix firstRedemptionOutputIdx . L._TxOut . L._2 . L._TxOutValue)
                      ( <>
                          ( C.lovelaceToValue $
                              C.quantityToLovelace $
                                C.Quantity $
                                  totalLovelaceFromRedemptionScript - totalLovelaceToRedemptionScript
                          )
                      )
                      txBody
                  else txBody
              Nothing -> txBody

      let subdivideRedeemer =
            RST.RedemptionSupplySubdivide
              subdivisionDepth
              (fmap (toPlutusMerkleHash . fst) subtreesEmpty)

      case redemptionSupplyRefTxIn of
        Just txIn ->
          spendPlutusRefWithInlineDatum
            redemptionSupplyTxIn
            txIn
            C.PlutusScriptV3
            subdivideRedeemer
        Nothing ->
          spendPlutusInlineDatum
            redemptionSupplyTxIn
            redemptionSupplyScript
            subdivideRedeemer

toPlutusMerkleHash :: ByteString -> MerkleHash
toPlutusMerkleHash h = MerkleHash (P.toBuiltin h)

data MaterializeRedemptionError era
  = MaterializeRedemptionDeserializationError DeserializeDatumError
  | MaterializeRedemptionAddressDeserializationError (C.AddressInEra era)
  | MaterializeRedemptionInvalidInputValue C.Value
  | MaterializeRedemptionWrongState (C.TxOutDatum C.CtxUTxO era)
  | MaterializeRedemptionThawingScheduleNotReduced
  deriving (Show)

materializeRedemptionTx
  :: ( MonadBuildTx era m
     , C.IsBabbageBasedEra era
     , C.HasScriptLanguageInEra C.PlutusScriptV3 era
     , MonadError (MaterializeRedemptionError era) m
     )
  => C.NetworkId
  -> C.LedgerProtocolParameters era
  -> C.TxIn -- redemptionSupplyTxIn
  -> C.TxOut C.CtxUTxO era -- redemptionSupplyInput
  -> C.PlutusScript C.PlutusScriptV3 -- redemptionSupplyScript
  -> Maybe C.TxIn -- maybe redemptionSupplyRefTxIn
  -> C.ScriptHash -- redemptionScriptHash (destination script)
  -> C.AddressInEra era -- payout destination (pubkey addr)
  -> C.AssetId -- nightAssetId
  -> Integer -- redemptionAmount (STAR to thaw now)
  -> Integer -- claimIndex
  -> TS.JitterStratum
  -> TS.JitterStrataCount
  -> RST.TotalIncrements
  -> [(ByteString, Integer)] -- membershipProof (hash, amount)
  -> POSIXTime
  -> m ()
materializeRedemptionTx
  networkId
  _ledgerProtocolParameters
  redemptionSupplyTxIn
  redemptionSupplyInput
  redemptionSupplyScript
  maybeRedemptionSupplyRefTxIn
  redemptionScriptHash
  payoutPkAddress
  nightAssetId
  redemptionAmount
  claimIndex
  jitterStratum
  jitterStrataCount
  (RST.TotalIncrements totalIncrements)
  rawProof
  currentTime = do
    -- Read and validate input datum; must be Materializing
    ( shardRootHash
      , genesisTimestamp
      , redemptionIncrementPeriod
      , materializationMask
      ) <-
      maybe
        (throwError (MaterializeRedemptionDeserializationError MissingInputDatum))
        ( maybe
            (throwError (MaterializeRedemptionDeserializationError InvalidInputDatum))
            ( \case
                RST.RedemptionSupplyDatum h t r (RST.Materializing m) -> pure (h, t, r, m)
                _ ->
                  throwError
                    ( MaterializeRedemptionWrongState (L.view (L._TxOut . L._3) redemptionSupplyInput)
                    )
            )
            . fromHashableScriptData @RST.RedemptionSupplyDatum
        )
        (L.preview (L._TxOut . L._3 . L._TxOutDatumInline) redemptionSupplyInput)

    let redemptionSupplyAddress = L.view (L._TxOut . L._1) redemptionSupplyInput
        inputValue = L.view (L._TxOut . L._2 . L._TxOutValue) redemptionSupplyInput
        (C.Quantity inputLovelace) = C.selectAsset inputValue C.AdaAssetId
        (C.Quantity inputStars) = C.selectAsset inputValue nightAssetId

    when (redemptionAmount <= 0 || redemptionAmount > inputStars) $
      throwError
        ( MaterializeRedemptionInvalidInputValue
            (C.txOutValueToValue (L.view (L._TxOut . L._2) redemptionSupplyInput))
        )

    --  Build continuation output at redemption supply address (mark claim; reduce STAR)
    let continuationDatum =
          C.TxOutDatumInline C.babbageBasedEra . toHashableScriptData $
            RST.RedemptionSupplyDatum
              shardRootHash
              genesisTimestamp
              redemptionIncrementPeriod
              (RST.Materializing (RSTTerms.markClaim claimIndex materializationMask))

        continuationValue =
          mkTxOutValue $
            fromList
              [ (C.AdaAssetId, C.Quantity inputLovelace)
              , (nightAssetId, C.Quantity (inputStars - redemptionAmount))
              ]

        continuationOut =
          C.TxOut
            redemptionSupplyAddress
            continuationValue
            continuationDatum
            C.ReferenceScriptNone

    addOutput continuationOut

    payoutAddressPlutus <-
      maybe
        (throwError (MaterializeRedemptionAddressDeserializationError payoutPkAddress))
        pure
        (PL.transAddressInEra payoutPkAddress)

    -- Build payout output at redemption script address with inline Redemption datum
    let initialThaw =
          TS.initialThaw
            genesisTimestamp
            redemptionIncrementPeriod
            jitterStratum
            jitterStrataCount
        reduction =
          TS.reduceThawingSchedule
            redemptionIncrementPeriod
            (TS.PastPoint $ PL.transPOSIXTime currentTime)
            (coerce @(Integer -> Integer -> Integer) div redemptionAmount totalIncrements)
            initialThaw
            (TS.IncrementsLeft totalIncrements)
    (redemptionDatum, thawedAmount) <- case reduction of
      TS.ThawingScheduleNotReduced -> throwError MaterializeRedemptionThawingScheduleNotReduced
      TS.ThawingSchedulePartiallyReduced
        thawed
        (TS.ThawTime nextThaw)
        (TS.IncrementsLeft incLeft) ->
          pure
            ( Just
                ( SR.RedemptionDatum
                    payoutAddressPlutus
                    (coerce redemptionAmount `div` totalIncrements)
                    nextThaw
                    incLeft
                    (coerce redemptionIncrementPeriod)
                )
            , coerce thawed
            )
      TS.ThawingScheduleFullyReduced -> pure (Nothing, redemptionAmount)

    let payoutValue = mkTxOutValue $ fromList [(nightAssetId, C.Quantity thawedAmount)]
        payoutOut = C.TxOut payoutPkAddress payoutValue C.TxOutDatumNone C.ReferenceScriptNone

    addOutput payoutOut

    case redemptionDatum of
      Just datum -> do
        let payoutStakeRef =
              maybe
                C.NoStakeAddress
                C.fromShelleyStakeReference
                (L.preview (L._AddressInEra . L._Address . L._3) payoutPkAddress)
            redemptionValue = fromList [(nightAssetId, C.Quantity thawedAmount)]
        payToScriptInlineDatum
          networkId
          redemptionScriptHash
          datum
          payoutStakeRef
          redemptionValue
      Nothing -> pure ()

    -- Spend the redemption supply shard with Materialize redeemer
    let proof = fmap (Data.Bifunctor.first toPlutusMerkleHash) rawProof
        redeemer =
          RST.RedemptionSupplyMaterialize
            RST.MaterializationInfo
              { RST.jitterStratum = TS.getJitterStratum jitterStratum
              , RST.amount = redemptionAmount
              , RST.membershipProof = proof
              , RST.claimIndex = claimIndex
              }

    case maybeRedemptionSupplyRefTxIn of
      Just refTxIn ->
        spendPlutusRefWithInlineDatum
          redemptionSupplyTxIn
          refTxIn
          C.PlutusScriptV3
          redeemer
      Nothing ->
        spendPlutusInlineDatum
          redemptionSupplyTxIn
          redemptionSupplyScript
          redeemer
