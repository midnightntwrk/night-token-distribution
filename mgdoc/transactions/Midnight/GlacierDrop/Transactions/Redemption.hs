module Midnight.GlacierDrop.Transactions.Redemption where

import Cardano.Api (
  Address,
  AddressInEra (AddressInEra),
  AddressTypeInEra (ShelleyAddressInEra),
  AlonzoEraOnwards (AlonzoEraOnwardsConway),
  BabbageEraOnwards (BabbageEraOnwardsConway),
  BuildTxWith (BuildTxWith),
  ConwayEra,
  EraHistory (..),
  KeyWitnessInCtx (KeyWitnessForSpending),
  NetworkId,
  Quantity (Quantity),
  ShelleyAddr,
  ShelleyBasedEra (ShelleyBasedEraConway),
  SystemStart,
  TxBody,
  TxBodyContent (
    txIns,
    txInsCollateral,
    txInsReference,
    txOuts,
    txProtocolParams,
    txValidityLowerBound,
    txValidityUpperBound
  ),
  TxBodyErrorAutoBalance,
  TxInsCollateral (TxInsCollateral),
  TxInsReference (TxInsReference),
  TxOut (..),
  TxOutDatum (TxOutDatumInline, TxOutDatumNone),
  Value,
  Witness (KeyWitness),
  defaultTxBodyContent,
  makeTransactionBodyAutoBalance,
  negateValue,
  shelleyAddressInEra,
  toLedgerEpochInfo,
  txOutValueToValue,
 )
import Cardano.Api qualified as C
import Cardano.Api.Shelley (
  LedgerProtocolParameters,
  ReferenceScript (ReferenceScriptNone),
  TxValidityUpperBound (..),
 )
import Control.Error (note)
import Data.Bifunctor (Bifunctor (first), bimap)
import Data.Foldable (Foldable (toList))
import Data.List.NonEmpty (NonEmpty)
import Data.Set qualified as S
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock.POSIX qualified as Clock
import GHC.IsList (IsList (fromList))
import Midnight.GlacierDrop.Contrib.Cardano.Api (
  AUTxO (AUTxO, aTxIn),
  foldToUTxO,
 )
import Midnight.GlacierDrop.Scripts.Redemption.Types (
  RedemptionDatum (RedemptionDatum),
  RedemptionRedeemer (Thaw),
 )
import Midnight.GlacierDrop.Scripts.ThawingSchedule qualified as TS
import Midnight.GlacierDrop.Transactions.Common (
  AddressScriptHashError,
  DeserializeDatumError,
  FromPlutusAddressError,
  SingleTokenValue (..),
  adjustForMinUTxO,
  clockDriftCompensation,
  clockPOSIXTimeToPlutusPOSIXTime,
  deserializeDatumInline,
  fromPlutusAddress,
  getAddressScriptHash,
  getTxBody,
  makeHashableScriptData,
  makeScriptWitness,
  parseSingleTokenValue,
  valueToTxOutValue,
 )
import Midnight.GlacierDrop.Transactions.RedemptionSupplyMaterialize (
  utcTimeToSlotNo,
 )
import PlutusLedgerApi.V1.Time qualified as P

data RedemptionTransactionArgs = RedemptionTransactionArgs
  { eraHistory :: EraHistory
  , feeUTxOs :: NonEmpty (AUTxO ConwayEra)
  , collateralUTxOs :: [AUTxO ConwayEra]
  , networkId :: NetworkId
  , now :: Clock.POSIXTime
  , protocolParams :: LedgerProtocolParameters ConwayEra
  , redemptionScriptUTxO :: AUTxO ConwayEra
  , redemptionUTxO :: AUTxO ConwayEra
  , extraKeyWits :: [C.Hash C.PaymentKey]
  , changeAddress :: Address ShelleyAddr
  , systemStart :: SystemStart
  }

data RedemptionError
  = TxBodyErrorAutoBalance (TxBodyErrorAutoBalance ConwayEra)
  | InvalidInputAssets Value
  | InrementBeginPastHorizon
  | NoRedeemableThaws {nextThaw :: P.POSIXTime, now :: P.POSIXTime}
  | AddressScriptHashError AddressScriptHashError
  | DeserializeDatumError DeserializeDatumError
  | ClaimantAddressDecodingError FromPlutusAddressError
  deriving (Show)

buildRedemptionTransaction
  :: RedemptionTransactionArgs -> Either RedemptionError (TxBody ConwayEra)
buildRedemptionTransaction args = bimap id fst (buildRedemptionTransactionWithThawValue args)

buildRedemptionTransactionWithThawValue
  :: RedemptionTransactionArgs -> Either RedemptionError (TxBody ConwayEra, Integer)
buildRedemptionTransactionWithThawValue args = do
  let RedemptionTransactionArgs
        { eraHistory
        , feeUTxOs = toList -> feeUTxOs
        , collateralUTxOs
        , networkId
        , now = subtract clockDriftCompensation -> now
        , protocolParams
        , redemptionScriptUTxO
        , redemptionUTxO
        , extraKeyWits
        , changeAddress
        , systemStart
        } = args
      epochInfo = toLedgerEpochInfo eraHistory
      AUTxO
        redemptionTxIn
        (TxOut redemptionAddress redemptionInputValue redemptionDatum _) = redemptionUTxO

  redemptionScriptHash <-
    first AddressScriptHashError . getAddressScriptHash $ redemptionAddress

  RedemptionDatum
    claimantAddress
    incrementAmountQuotient
    prevThaw
    incrementsLeft
    redemptionIncrementPeriod <-
    first DeserializeDatumError . deserializeDatumInline $ redemptionDatum

  claimantShelleyAddress <-
    first ClaimantAddressDecodingError $ fromPlutusAddress claimantAddress networkId

  inputValue <- do
    let value = txOutValueToValue redemptionInputValue
    note (InvalidInputAssets value) $
      parseSingleTokenValue value

  let SingleTokenValue _ nightAssetId inputStarAmount = inputValue

  possibleContinuationInfo
    :: Maybe (TS.StarAmount, TS.ThawTime, TS.IncrementsLeft) <- do
    let plutusNow = clockPOSIXTimeToPlutusPOSIXTime now
        pastPoint = TS.PastPoint plutusNow
        thawTime = TS.ThawTime prevThaw
    case TS.reduceThawingSchedule
      redemptionIncrementPeriod
      pastPoint
      (TS.StarAmount incrementAmountQuotient)
      thawTime
      (TS.IncrementsLeft incrementsLeft) of
      TS.ThawingScheduleNotReduced -> Left $ NoRedeemableThaws{nextThaw = prevThaw, now = plutusNow}
      TS.ThawingSchedulePartiallyReduced starAmount' thawTime' incrementsLeft' -> pure $ Just (starAmount', thawTime', incrementsLeft')
      TS.ThawingScheduleFullyReduced -> pure Nothing

  incrementBeginSlot <-
    fmap succ
      . (first . const $ InrementBeginPastHorizon)
      . utcTimeToSlotNo systemStart eraHistory
      . posixSecondsToUTCTime
      $ now

  let thawAmount = case possibleContinuationInfo of
        Just (TS.StarAmount starAmount, _, _) -> starAmount
        Nothing -> inputStarAmount
      thawValue = fromList [(nightAssetId, Quantity thawAmount)]
      txOuts =
        [ adjustForMinUTxO id protocolParams $
            TxOut
              (shelleyAddressInEra ShelleyBasedEraConway claimantShelleyAddress)
              (valueToTxOutValue thawValue)
              TxOutDatumNone
              ReferenceScriptNone
        ]
          <> case possibleContinuationInfo of
            Just (_, TS.ThawTime nextThaw, TS.IncrementsLeft incrementsLeft') ->
              [ TxOut
                  redemptionAddress
                  ( valueToTxOutValue $
                      txOutValueToValue redemptionInputValue <> negateValue thawValue
                  )
                  ( TxOutDatumInline BabbageEraOnwardsConway . makeHashableScriptData $
                      RedemptionDatum
                        claimantAddress
                        incrementAmountQuotient
                        nextThaw
                        incrementsLeft'
                        redemptionIncrementPeriod
                  )
                  ReferenceScriptNone
              ]
            Nothing -> []

      utxoSet =
        foldToUTxO $
          [ redemptionScriptUTxO
          , redemptionUTxO
          ]
            <> feeUTxOs

      txBody = do
        let AUTxO redemptionScriptTxIn _ = redemptionScriptUTxO
        (defaultTxBodyContent ShelleyBasedEraConway)
          { txIns =
              ( redemptionTxIn
              , makeScriptWitness redemptionScriptTxIn redemptionScriptHash Thaw
              )
                : ( (\(AUTxO aTxIn _) -> (aTxIn, BuildTxWith $ KeyWitness $ KeyWitnessForSpending))
                      <$> feeUTxOs
                  )
          , txInsCollateral =
              TxInsCollateral AlonzoEraOnwardsConway $
                aTxIn <$> (if null collateralUTxOs then feeUTxOs else collateralUTxOs)
          , txInsReference =
              TxInsReference
                BabbageEraOnwardsConway
                [redemptionScriptTxIn]
                (C.BuildTxWith S.empty)
          , txOuts
          , txProtocolParams = BuildTxWith (Just protocolParams)
          , txValidityLowerBound =
              C.TxValidityLowerBound C.AllegraEraOnwardsConway incrementBeginSlot
          , txValidityUpperBound = TxValidityUpperBound ShelleyBasedEraConway Nothing
          , C.txExtraKeyWits =
              C.TxExtraKeyWitnesses C.AlonzoEraOnwardsConway extraKeyWits
          }

  fmap (\t -> (getTxBody t, thawAmount))
    . first TxBodyErrorAutoBalance
    $ makeTransactionBodyAutoBalance
      ShelleyBasedEraConway
      systemStart
      epochInfo
      protocolParams
      mempty
      mempty
      mempty
      utxoSet
      txBody
      (AddressInEra (ShelleyAddressInEra ShelleyBasedEraConway) changeAddress)
      Nothing
