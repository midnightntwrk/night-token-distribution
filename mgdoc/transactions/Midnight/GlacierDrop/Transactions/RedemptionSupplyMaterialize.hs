module Midnight.GlacierDrop.Transactions.RedemptionSupplyMaterialize where

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley qualified as C
import Cardano.Api.Shelley qualified as CS
import Control.Error.Util (note)
import Data.Bifunctor (Bifunctor (first), bimap)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Foldable (Foldable (toList))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (maybeToList)
import Data.Set qualified as S
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock.POSIX qualified as Clock
import GHC.IsList (IsList (fromList))
import Midnight.GlacierDrop.Api qualified as S
import Midnight.GlacierDrop.Contrib.Cardano.Api (
  AUTxO (AUTxO, aTxIn),
  foldToUTxO,
 )
import Midnight.GlacierDrop.Contrib.ShowCBORHex (ShowCBORHex (ShowCBORHex))
import Midnight.GlacierDrop.Scripts.Redemption.Types qualified as SR
import Midnight.GlacierDrop.Scripts.RedemptionSupply.Terms qualified as S
import Midnight.GlacierDrop.Scripts.RedemptionSupply.Types qualified as S
import Midnight.GlacierDrop.Scripts.ThawingSchedule qualified as TS
import Midnight.GlacierDrop.Transactions.Common (
  AddressScriptHashError (..),
  DeserializeDatumError (..),
  SingleTokenValue (..),
  adjustForMinUTxO,
  clockDriftCompensation,
  clockPOSIXTimeToPlutusPOSIXTime,
  deserializeDatumInline,
  getAddressScriptHash,
  getTxBody,
  makeHashableScriptData,
  makeScriptWitness,
  mkAddressInConwayEra,
  parseSingleTokenValue,
  scriptHashFromReferenceInput,
  toPlutusAddress,
  validationFailureScriptContexts,
  valueToTxOutValue,
 )
import Ouroboros.Consensus.BlockchainTime (toRelativeTime)
import Ouroboros.Consensus.HardFork.History (
  PastHorizonException,
  interpretQuery,
  wallclockToSlot,
 )
import PlutusLedgerApi.Common qualified as P

newtype RedemptionDestinationAddress = RedemptionDestinationAddress
  {getRedemptionDestinationAddress :: C.Address C.ShelleyAddr}

data RedemptionSupplyMaterializeArgs = RedemptionSupplyMaterializeArgs
  { eraHistory :: C.EraHistory
  , feeUTxOs :: NonEmpty (AUTxO C.ConwayEra)
  -- ^ The funding UTxO for the transaction fee also used as collateral
  , jitterStrataCount :: TS.JitterStrataCount
  , networkId :: C.NetworkId
  , nightAssetId :: C.AssetId
  , now :: Clock.POSIXTime
  , protocolParams :: C.LedgerProtocolParameters C.ConwayEra
  , redemptionAmount :: RedemptionAmount
  , redemptionDestinationAddress :: RedemptionDestinationAddress
  , redemptionJitterStratum :: TS.JitterStratum
  , redemptionMembershipProof :: [(ByteString, Integer)]
  , redemptionScriptUTxO :: AUTxO C.ConwayEra
  , redemptionShardIndex :: RedemptionShardIndex
  , redemptionSupplyScriptUTxO :: AUTxO C.ConwayEra
  , redemptionSupplyUTxO :: AUTxO C.ConwayEra
  , redemptionTotalIncrements :: S.TotalIncrements
  , extraKeyWits :: [C.Hash C.PaymentKey]
  -- ^ The actual redemption tree UTxO which we divide
  , changeAddress :: C.Address C.ShelleyAddr
  , systemStart :: C.SystemStart
  }

data RedemptionSupplyMaterializeError
  = TxBodyErrorAutoBalance (C.TxBodyErrorAutoBalance C.ConwayEra) String
  | InvalidInputAssets C.Value
  | InvalidRedemptionScript
  | IncrementBeginPastHorizon
  | DeserializeDatumError DeserializeDatumError
  | InvalidRedemptionSupplyScriptAddress AddressScriptHashError
  | InvalidRedemptionSupplyAddress
  | ThawingScheduleNotReduced
  | RedemptionSupplyDatumWrongState
  deriving (Show)

buildRedemptionSupplyMaterializeTransaction
  :: RedemptionSupplyMaterializeArgs
  -> Either RedemptionSupplyMaterializeError (C.TxBody C.ConwayEra)
buildRedemptionSupplyMaterializeTransaction args = bimap id fst (buildRedemptionSupplyMaterializeTransactionWithThawValue args)

buildRedemptionSupplyMaterializeTransactionWithThawValue
  :: RedemptionSupplyMaterializeArgs
  -> Either RedemptionSupplyMaterializeError (C.TxBody C.ConwayEra, Integer)
buildRedemptionSupplyMaterializeTransactionWithThawValue args = do
  let RedemptionSupplyMaterializeArgs
        { eraHistory
        , feeUTxOs = toList -> feeUTxOs
        , jitterStrataCount
        , networkId
        , nightAssetId
        , now = subtract clockDriftCompensation -> now
        , protocolParams
        , redemptionAmount
        , redemptionDestinationAddress
        , redemptionJitterStratum
        , redemptionMembershipProof
        , redemptionScriptUTxO
        , redemptionShardIndex
        , redemptionSupplyScriptUTxO
        , redemptionSupplyUTxO
        , redemptionTotalIncrements
        , extraKeyWits
        , changeAddress
        , systemStart
        } = args
      AUTxO
        redemptionSupplyTxIn
        ( C.TxOut
            redemptionSupplyAddressInEra
            redemptionSupplyValue
            redemptionSupplyDatum
            _
          ) = redemptionSupplyUTxO

  redemptionSupplyAddress <-
    note InvalidRedemptionSupplyAddress $
      fromAddresInEra redemptionSupplyAddressInEra

  ( shardRootHash
    , genesisTimestamp
    , redemptionIncrementPeriod
    , materializationMask
    ) <-
    first
      DeserializeDatumError
      (deserializeDatumInline redemptionSupplyDatum)
      >>= \case
        S.RedemptionSupplyDatum h t r (S.Materializing m) -> pure (h, t, r, m)
        _ -> Left RedemptionSupplyDatumWrongState

  inputValue <- do
    let value = C.txOutValueToValue redemptionSupplyValue
    note (InvalidInputAssets value) $
      parseSingleTokenValue value

  let -- We put together redemption script address using two pieces:
      -- \* script address based on the script hash from the redemption script UTxO
      -- \* staking part from destination address
      AUTxO _ redemptionScriptTxOut = redemptionScriptUTxO

  redemptionScriptHash <-
    note InvalidRedemptionScript $
      scriptHashFromReferenceInput redemptionScriptTxOut

  let RedemptionDestinationAddress (CS.ShelleyAddress _ _ stakeReference) = redemptionDestinationAddress
      redemptionScriptAddress =
        CS.makeShelleyAddress
          networkId
          (C.PaymentCredentialByScript redemptionScriptHash)
          $ C.fromShelleyStakeReference stakeReference

  let pastPoint = TS.PastPoint . clockPOSIXTimeToPlutusPOSIXTime $ now

  (redemptionOutputs, thawedAmount) <-
    mkRedemptionOutputs
      redemptionJitterStratum
      jitterStrataCount
      genesisTimestamp
      redemptionIncrementPeriod
      redemptionTotalIncrements
      redemptionAmount
      redemptionDestinationAddress
      (RedemptionScriptAddress redemptionScriptAddress)
      pastPoint
      nightAssetId
      protocolParams

  let membershipProof = first (S.MerkleHash . P.toBuiltin) <$> redemptionMembershipProof
      redeemer =
        S.RedemptionSupplyMaterialize $
          S.MaterializationInfo
            { jitterStratum = TS.getJitterStratum redemptionJitterStratum
            , amount = TS.getStarAmount . getRedemptionAmount $ redemptionAmount
            , membershipProof
            , claimIndex = getRedemptionShardIndex redemptionShardIndex
            }

      redemptionShardOutput =
        mkRedemptionShardOutput
          shardRootHash
          materializationMask
          genesisTimestamp
          redemptionIncrementPeriod
          redemptionAmount
          redemptionShardIndex
          (RedemptionSupplyInputValue inputValue)
          (RedemptionSupplyScriptAddress redemptionSupplyAddress)
          protocolParams

      AUTxO redemptionSupplyScriptTxIn _ = redemptionSupplyScriptUTxO
      txOuts = redemptionShardOutput : redemptionOutputs

  redemptionSupplyScriptHash <-
    first InvalidRedemptionSupplyScriptAddress $
      getAddressScriptHash redemptionSupplyAddressInEra

  incrementBeginSlot <-
    fmap succ
      . (first . const $ IncrementBeginPastHorizon)
      . utcTimeToSlotNo systemStart eraHistory
      . posixSecondsToUTCTime
      $ now

  let txBodyContent =
        (C.defaultTxBodyContent C.ShelleyBasedEraConway)
          { C.txIns =
              ( redemptionSupplyTxIn
              , makeScriptWitness redemptionSupplyScriptTxIn redemptionSupplyScriptHash redeemer
              )
                : ( ( \(AUTxO aTxIn _) -> (aTxIn, C.BuildTxWith $ C.KeyWitness $ C.KeyWitnessForSpending)
                    )
                      <$> feeUTxOs
                  )
          , C.txInsCollateral =
              C.TxInsCollateral C.AlonzoEraOnwardsConway $ aTxIn <$> feeUTxOs
          , C.txInsReference =
              C.TxInsReference
                C.BabbageEraOnwardsConway
                [redemptionSupplyScriptTxIn]
                (C.BuildTxWith S.empty)
          , C.txOuts = txOuts
          , C.txProtocolParams = C.BuildTxWith (Just protocolParams)
          , C.txValidityLowerBound =
              C.TxValidityLowerBound C.AllegraEraOnwardsConway incrementBeginSlot
          , C.txValidityUpperBound = C.TxValidityUpperBound C.ShelleyBasedEraConway Nothing
          , C.txExtraKeyWits =
              C.TxExtraKeyWitnesses C.AlonzoEraOnwardsConway extraKeyWits
          }
      utxoSet =
        foldToUTxO $
          [ redemptionSupplyScriptUTxO
          , redemptionSupplyUTxO
          ]
            <> feeUTxOs

  fmap (\t -> (getTxBody t, thawedAmount))
    . first
      ( \err ->
          TxBodyErrorAutoBalance err $
            show err
              <> "\n"
              <> show
                ( ShowCBORHex
                    <$> validationFailureScriptContexts
                      C.ShelleyBasedEraConway
                      systemStart
                      (C.toLedgerEpochInfo eraHistory)
                      protocolParams
                      utxoSet
                      txBodyContent
                      (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraConway) changeAddress)
                )
      )
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
      (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraConway) changeAddress)
      Nothing

newtype RedemptionAmount = RedemptionAmount {getRedemptionAmount :: TS.StarAmount}

newtype RedemptionShardIndex = RedemptionShardIndex {getRedemptionShardIndex :: Integer}

newtype RedemptionSupplyScriptAddress = RedemptionSupplyScriptAddress
  {getRedemptionSupplyScriptAddress :: C.Address C.ShelleyAddr}

newtype RedemptionSupplyInputValue = RedemptionSupplyInputValue SingleTokenValue

mkRedemptionShardOutput
  :: S.MerkleHash
  -> S.MaterializationMask
  -> TS.GenesisTimestamp
  -> TS.RedemptionIncrementPeriod
  -> RedemptionAmount
  -> RedemptionShardIndex
  -> RedemptionSupplyInputValue
  -> RedemptionSupplyScriptAddress
  -> C.LedgerProtocolParameters C.ConwayEra
  -> C.TxOut C.CtxTx C.ConwayEra
mkRedemptionShardOutput
  shardRootHash
  materializationMask
  genesisTimestamp
  redemptionIncrementPeriod
  (RedemptionAmount (TS.StarAmount redemptionAmount))
  (RedemptionShardIndex index)
  (RedemptionSupplyInputValue inputValue)
  (RedemptionSupplyScriptAddress outputAddress)
  protocolParams = do
    let outputDatum = do
          let materializationMask' = S.markClaim index materializationMask
              datumValue =
                S.RedemptionSupplyDatum
                  shardRootHash
                  genesisTimestamp
                  redemptionIncrementPeriod
                  (S.Materializing materializationMask')
          C.TxOutDatumInline C.BabbageEraOnwardsConway . makeHashableScriptData $
            datumValue

        outputValue = do
          let SingleTokenValue (C.Quantity lovelace) nightAssetId inputStarAmount = inputValue
              outputStarAmount = inputStarAmount - redemptionAmount
          valueToTxOutValue $
            C.lovelaceToValue (L.Coin lovelace)
              <> fromList [(nightAssetId, C.Quantity outputStarAmount)]

    adjustForMinUTxO id protocolParams $
      C.TxOut
        (mkAddressInConwayEra outputAddress)
        outputValue
        outputDatum
        C.ReferenceScriptNone

mkRedemptionDatum
  :: S.TotalIncrements
  -> TS.GenesisTimestamp
  -> TS.RedemptionIncrementPeriod
  -> TS.StarAmount
  -> RedemptionDestinationAddress
  -> TS.JitterStratum
  -> TS.JitterStrataCount
  -> TS.PastPoint
  -> Either
      RedemptionSupplyMaterializeError
      (Maybe SR.RedemptionDatum, TS.StarAmount)
mkRedemptionDatum
  (S.TotalIncrements totalIncrements)
  genesisTimestamp
  incrementPeriod
  redemptionAmount
  (RedemptionDestinationAddress outputAddress)
  jitterStratum
  jitterStrataCount
  now = do
    let initialThaw =
          TS.initialThaw genesisTimestamp incrementPeriod jitterStratum jitterStrataCount
        reduction =
          TS.reduceThawingSchedule
            incrementPeriod
            now
            (coerce @(Integer -> Integer -> Integer) div redemptionAmount totalIncrements)
            initialThaw
            (TS.IncrementsLeft totalIncrements)
    case reduction of
      TS.ThawingScheduleNotReduced -> Left ThawingScheduleNotReduced
      TS.ThawingSchedulePartiallyReduced
        thawedAmount
        (TS.ThawTime nextThaw)
        (TS.IncrementsLeft incrementsLeft) -> do
          let outputAddress' = toPlutusAddress outputAddress
          pure
            ( Just $
                SR.RedemptionDatum
                  outputAddress'
                  (coerce redemptionAmount `div` totalIncrements)
                  nextThaw
                  incrementsLeft
                  (coerce incrementPeriod)
            , thawedAmount
            )
      TS.ThawingScheduleFullyReduced -> pure (Nothing, redemptionAmount)

data RedemptionOutputs = RedemptionOutputs
  { payoutOutput :: C.TxOut C.CtxTx C.ConwayEra
  , redemptionOutput :: Maybe (C.TxOut C.CtxTx C.ConwayEra)
  }

newtype RedemptionScriptAddress = RedemptionScriptAddress (C.Address C.ShelleyAddr)

fromAddresInEra :: C.AddressInEra era -> Maybe (C.Address C.ShelleyAddr)
fromAddresInEra (C.AddressInEra (C.ShelleyAddressInEra _) addr) = Just addr
fromAddresInEra _ = Nothing

mkRedemptionOutputs
  :: TS.JitterStratum
  -> TS.JitterStrataCount
  -> TS.GenesisTimestamp
  -> TS.RedemptionIncrementPeriod
  -> S.TotalIncrements
  -> RedemptionAmount
  -> RedemptionDestinationAddress
  -> RedemptionScriptAddress
  -> TS.PastPoint
  -> C.AssetId
  -> C.LedgerProtocolParameters C.ConwayEra
  -> Either RedemptionSupplyMaterializeError ([C.TxOut C.CtxTx C.ConwayEra], Integer)
mkRedemptionOutputs
  jitterStratum
  jitterStrataCount
  genesisTimestamp
  redemptionIncrementPeriod
  totalIncrements
  (RedemptionAmount redemptionAmount)
  payoutAddress@(RedemptionDestinationAddress payoutAddressAddr)
  (RedemptionScriptAddress scriptAddress)
  now
  nightAssetId
  protocolParams = do
    (redemptionDatum, TS.StarAmount thawedAmount) <-
      mkRedemptionDatum
        totalIncrements
        genesisTimestamp
        redemptionIncrementPeriod
        redemptionAmount
        payoutAddress
        jitterStratum
        jitterStrataCount
        now

    let payoutOutput = do
          let payoutValue =
                valueToTxOutValue . fromList $
                  [(nightAssetId, C.Quantity thawedAmount)]
          adjustForMinUTxO id protocolParams $
            C.TxOut
              (mkAddressInConwayEra payoutAddressAddr)
              payoutValue
              C.TxOutDatumNone
              C.ReferenceScriptNone

        possibleRedemptionOutput = do
          datum <- redemptionDatum
          let value = valueToTxOutValue . fromList $ do
                let TS.StarAmount total = redemptionAmount
                    remaining = total - thawedAmount
                [(nightAssetId, C.Quantity remaining)]
          pure . adjustForMinUTxO id protocolParams $
            C.TxOut
              (mkAddressInConwayEra scriptAddress)
              value
              (C.TxOutDatumInline C.BabbageEraOnwardsConway . makeHashableScriptData $ datum)
              C.ReferenceScriptNone

    pure $ (payoutOutput : maybeToList possibleRedemptionOutput, thawedAmount)

utcTimeToSlotNo
  :: C.SystemStart
  -> C.EraHistory
  -> UTCTime
  -> Either PastHorizonException CS.SlotNo
utcTimeToSlotNo systemStart (C.EraHistory interpreter) time = do
  let relativeTime = toRelativeTime systemStart time
  (slotNo, _, _) <-
    interpretQuery interpreter $
      wallclockToSlot relativeTime
  pure slotNo
