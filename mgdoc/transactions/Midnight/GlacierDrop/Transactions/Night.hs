module Midnight.GlacierDrop.Transactions.Night where

import Cardano.Api (
  Address,
  AddressInEra (AddressInEra),
  AddressTypeInEra (ShelleyAddressInEra),
  AllegraEraOnwards (AllegraEraOnwardsConway),
  AlonzoEraOnwards (AlonzoEraOnwardsConway),
  AssetId (AssetId),
  AssetName (..),
  BabbageEraOnwards (BabbageEraOnwardsConway),
  BuildTxWith (BuildTxWith),
  ConwayEra,
  CtxUTxO,
  EraHistory,
  ExecutionUnits (ExecutionUnits),
  Hash,
  KeyWitnessInCtx (KeyWitnessForSpending),
  MaryEraOnwards (MaryEraOnwardsConway),
  NetworkId,
  PaymentCredential (PaymentCredentialByScript),
  PaymentKey,
  PlutusScriptVersion (PlutusScriptV3),
  PolicyAssets (..),
  PolicyId (PolicyId),
  Quantity (Quantity),
  ScriptDatum (NoScriptDatumForMint),
  ScriptHash,
  ScriptLanguageInEra (PlutusScriptV3InConway),
  ScriptWitness (PlutusScriptWitness),
  SerialiseAsRawBytesError,
  ShelleyAddr,
  ShelleyBasedEra (ShelleyBasedEraConway),
  StakeAddressReference,
  SystemStart,
  TxBody,
  TxBodyContent (
    txExtraKeyWits,
    txIns,
    txInsCollateral,
    txInsReference,
    txMintValue,
    txOuts,
    txProtocolParams,
    txValidityLowerBound,
    txValidityUpperBound
  ),
  TxBodyErrorAutoBalance,
  TxExtraKeyWitnesses (TxExtraKeyWitnesses),
  TxIn,
  TxInsCollateral (TxInsCollateral),
  TxInsReference (TxInsReference),
  TxOut (..),
  TxOutDatum (TxOutDatumInline, TxOutDatumNone),
  TxValidityLowerBound (TxValidityLowerBound),
  TxValidityUpperBound (TxValidityUpperBound),
  UTxO (UTxO),
  Witness (KeyWitness),
  defaultTxBodyContent,
  makeShelleyAddress,
  makeTransactionBodyAutoBalance,
  mkTxMintValue,
  selectAsset,
  toLedgerEpochInfo,
  txOutValueToValue,
 )
import Cardano.Api.Shelley (
  LedgerProtocolParameters,
  PlutusScriptOrReferenceInput (PReferenceScript),
  ReferenceScript (ReferenceScriptNone),
 )
import Control.Error (note)
import Control.Lens qualified as L
import Control.Monad (when)
import Convex.CardanoApi.Lenses qualified as L
import Convex.PlutusLedger.V1 qualified as PL
import Data.Bifunctor (Bifunctor (first, second))
import Data.Coerce (coerce)
import Data.Foldable qualified as Foldable
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Time (NominalDiffTime, UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Clock.POSIX qualified as Clock
import GHC.IsList (IsList (fromList, toList))
import Midnight.GlacierDrop.Api (Star (Star))
import Midnight.GlacierDrop.Scripts.Common qualified as C
import Midnight.GlacierDrop.Scripts.HydraThread.Types (
  HydraThreadDatum (HydraThreadFinal),
  HydraThreadFinal (
    MakeHydraThreadFinal,
    redemptionsSum,
    redemptionsTreeDepth,
    redemptionsTreeRoot
  ),
  HydraThreadRedeemer (HydraThreadGenesis),
 )
import Midnight.GlacierDrop.Scripts.Night.Types (
  NightGenesis (NightGenesis),
  NightMintRedeemer (NightBurnGenesis),
 )
import Midnight.GlacierDrop.Scripts.ProtocolParams.Types qualified as NP
import Midnight.GlacierDrop.Scripts.RedemptionSupply.Types (
  RedemptionSupplyDatum (RedemptionSupplyDatum),
  RedemptionSupplyState (Subdividing),
 )
import Midnight.GlacierDrop.Scripts.ThawingSchedule (
  GenesisTimestamp (GenesisTimestamp),
 )
import Midnight.GlacierDrop.Scripts.ThawingSchedule qualified as TS
import Midnight.GlacierDrop.Transactions.Common (
  AddressScriptHashError,
  DeserializeDatumError,
  adjustForMinUTxO,
  clockDriftCompensation,
  deserializeDatumInline,
  getAddressScriptHash,
  getTxBody,
  makeHashableScriptData,
  makeScriptWitness,
  toPlutusScriptHash,
  txInsReferenceDatumsFromDatum,
  utcTimeToPOSIXTime,
  valueToTxOutValue,
 )
import Midnight.GlacierDrop.Transactions.RedemptionSupplyMaterialize (
  utcTimeToSlotNo,
 )
import Numeric.Natural (Natural)
import PlutusLedgerApi.V3 qualified as P

data NightError
  = TxBodyErrorAutoBalance (TxBodyErrorAutoBalance ConwayEra)
  | AddressScriptHashError AddressScriptHashError
  | DeserializeDatumError DeserializeDatumError
  | NightProtocolParamsInvalidDatum (TxOutDatum CtxUTxO ConwayEra)
  | DeserializeNightParamsDatumError DeserializeDatumError
  | DeserializeFoundationWalletError SerialiseAsRawBytesError
  | DeserializeRedemptionSupplyAddressError SerialiseAsRawBytesError
  | WrongHydraState
  | PosixPastHorizon
  | InvalidGenesisTimestamp
  deriving (Show)

data NightBurnGenesisArgs = NightBurnGenesisArgs
  { nightTxIn :: TxIn
  , nightInput :: TxOut CtxUTxO ConwayEra
  , hydraTxIn :: TxIn
  , hydraInput :: TxOut CtxUTxO ConwayEra
  , nightScriptTxIn :: TxIn
  , nightScriptInput :: TxOut CtxUTxO ConwayEra
  , hydraThreadScriptTxIn :: TxIn
  , hydraThreadScriptInput :: TxOut CtxUTxO ConwayEra
  , nightParamsTxIn :: TxIn
  , nightParamsInput :: TxOut CtxUTxO ConwayEra
  , feeTxIn :: TxIn
  , feeInput :: TxOut CtxUTxO ConwayEra
  , redemptionSupplyScriptHash :: ScriptHash
  , treasuryScriptHash :: ScriptHash
  , stakeAddress :: StakeAddressReference
  , foundationWallets :: Map (Address ShelleyAddr) Natural
  , minTreasuryTokens :: Natural
  , genesisTimestamp :: UTCTime
  , now :: Clock.POSIXTime
  , validityIntervalLength :: Maybe NominalDiffTime
  , redemptionIncrementPeriod :: TS.RedemptionIncrementPeriod
  , tgeAgentAuthKeys :: Set (Hash PaymentKey)
  -- ^ This is the set of required signers for the transaction. It should be a subset of the tgeAgentAuthKeys, where the size of the subset
  -- is greater than or equal to the minAuthSignatures.
  , tgeWallet :: Address ShelleyAddr
  , systemStart :: SystemStart
  , eraHistory :: EraHistory
  , protocolParams :: LedgerProtocolParameters ConwayEra
  , networkId :: NetworkId
  }

buildNightBurnGenesisTransaction
  :: NightBurnGenesisArgs -> Either NightError (TxBody ConwayEra)
buildNightBurnGenesisTransaction args = do
  let NightBurnGenesisArgs
        { nightTxIn
        , nightInput
        , hydraTxIn
        , hydraInput
        , nightScriptTxIn
        , nightScriptInput
        , hydraThreadScriptTxIn
        , hydraThreadScriptInput
        , nightParamsTxIn
        , nightParamsInput
        , feeTxIn
        , feeInput
        , redemptionSupplyScriptHash = _
        , treasuryScriptHash
        , stakeAddress
        , foundationWallets = _
        , minTreasuryTokens = fromIntegral -> minTreasuryTokens
        , genesisTimestamp
        , now = subtract clockDriftCompensation -> now
        , validityIntervalLength
        , redemptionIncrementPeriod
        , tgeAgentAuthKeys
        , tgeWallet
        , systemStart
        , eraHistory
        , protocolParams
        , networkId
        } = args
      TxOut nightAddress (txOutValueToValue -> nightInputValue) _ _ = nightInput
      TxOut hydraAddress (txOutValueToValue -> hydraValue) hydraDatum _ = hydraInput
  nightScriptHash <-
    first AddressScriptHashError $ getAddressScriptHash nightAddress
  hydraThreadScriptHash <-
    first AddressScriptHashError $ getAddressScriptHash hydraAddress
  NP.NightMintingParams
    { NP.foundationWallets = foundationWalletsDynamic'
    , NP.supplyScriptAddress = supplyScriptAddress'
    } <-
    (first DeserializeNightParamsDatumError . deserializeDatumInline) $
      L.view (L._TxOut . L._3) nightParamsInput
  foundationWalletsDynamic <-
    M.toAscList . M.fromList
      <$> traverse
        ( \(addr, amount) ->
            fmap (,amount) $
              first DeserializeFoundationWalletError $
                PL.unTransAddressShelley networkId addr
        )
        foundationWalletsDynamic'
  redemptionSupplyScriptAddress <-
    either (Left . DeserializeRedemptionSupplyAddressError) Right $
      PL.unTransAddressShelley networkId supplyScriptAddress'

  MakeHydraThreadFinal
    { redemptionsTreeRoot
    , redemptionsTreeDepth
    , redemptionsSum = coerce -> redemptionsSum
    } <-
    (=<<) (\case (HydraThreadFinal a) -> Right a; _ -> Left WrongHydraState)
      . (first DeserializeDatumError . deserializeDatumInline)
      $ hydraDatum
  let starAssetId = AssetId (PolicyId nightScriptHash) (AssetName "")
      Quantity starIn = selectAsset nightInputValue starAssetId
      claimTarget = div starIn 5 -- want 20% to claim + scavenge + lost&found
      minPostClaim = div starIn 100 -- 1% minimum (each) to scavenge and lost&found
      postClaim = max minPostClaim (div (claimTarget - redemptionsSum) 2)
      effectiveRedemptionsSum = redemptionsSum + postClaim
      starRemaining = starIn - effectiveRedemptionsSum
      foundationTotalAdjusted = starRemaining - minTreasuryTokens
      foundationTotalUnadjusted = sum (snd <$> foundationWalletsDynamic)
      foundationWalletsAdjusted
        | foundationTotalAdjusted <= 0 = mempty
        | foundationTotalAdjusted < foundationTotalUnadjusted =
            (map . second)
              ((`div` foundationTotalUnadjusted) . (* foundationTotalAdjusted))
              foundationWalletsDynamic
        | otherwise = foundationWalletsDynamic
      treasuryTokens = starRemaining - sum (snd <$> foundationWalletsAdjusted)
      hydraThreadToken = case find
        ( \case
            (AssetId policyId (AssetName assetName), _) ->
              policyId == PolicyId nightScriptHash
                && C.isHydraThreadToken (P.TokenName $ P.toBuiltin assetName)
            _ -> False
        )
        (toList hydraValue) of
        Just (AssetId _ n, _) -> n
        _ -> error "couldn't find hydra thread token"
  let twelveHours = 12 * 60 * 60
      genesisTimestampPOSIX = utcTimeToPOSIXSeconds genesisTimestamp
      genesisTimestampPlutusPOSIX = utcTimeToPOSIXTime genesisTimestamp
      posixToSlot =
        (first . const $ PosixPastHorizon)
          . utcTimeToSlotNo systemStart eraHistory
          . posixSecondsToUTCTime

  when (genesisTimestampPOSIX <= now + fromMaybe 1 validityIntervalLength) do
    Left InvalidGenesisTimestamp

  incrementBeginSlot <- fmap succ . posixToSlot $ now
  upperBoundSlot <-
    posixToSlot
      . fromMaybe (min (genesisTimestampPOSIX - 1) (now + twelveHours))
      . fmap (now +)
      $ validityIntervalLength

  _txInsReferenceDatums <-
    note
      (NightProtocolParamsInvalidDatum $ L.view (L._TxOut . L._3) nightParamsInput)
      . txInsReferenceDatumsFromDatum
      $ L.view (L._TxOut . L._3) nightParamsInput
  fmap getTxBody
    . first TxBodyErrorAutoBalance
    $ makeTransactionBodyAutoBalance
      ShelleyBasedEraConway
      systemStart
      (toLedgerEpochInfo eraHistory)
      protocolParams
      mempty
      mempty
      mempty
      ( UTxO $
          M.fromList $
            [ (nightTxIn, nightInput)
            , (hydraTxIn, hydraInput)
            , (nightScriptTxIn, nightScriptInput)
            , (hydraThreadScriptTxIn, hydraThreadScriptInput)
            , (feeTxIn, feeInput)
            , (nightParamsTxIn, nightParamsInput)
            ]
      )
      ( (defaultTxBodyContent ShelleyBasedEraConway)
          { txValidityLowerBound =
              TxValidityLowerBound AllegraEraOnwardsConway incrementBeginSlot
          , txValidityUpperBound =
              TxValidityUpperBound ShelleyBasedEraConway (Just upperBoundSlot)
          , txIns =
              [ (feeTxIn, BuildTxWith $ KeyWitness $ KeyWitnessForSpending)
              , (nightTxIn, makeScriptWitness nightScriptTxIn nightScriptHash ())
              ,
                ( hydraTxIn
                , makeScriptWitness hydraThreadScriptTxIn hydraThreadScriptHash HydraThreadGenesis
                )
              ]
          , txInsCollateral = TxInsCollateral AlonzoEraOnwardsConway [feeTxIn]
          , txInsReference =
              TxInsReference
                BabbageEraOnwardsConway
                [nightScriptTxIn, hydraThreadScriptTxIn, nightParamsTxIn]
                (BuildTxWith mempty)
          , txOuts =
              adjustForMinUTxO id protocolParams
                <$> TxOut
                  ( AddressInEra
                      (ShelleyAddressInEra ShelleyBasedEraConway)
                      redemptionSupplyScriptAddress
                  )
                  ( valueToTxOutValue $ fromList $ [(starAssetId, Quantity effectiveRedemptionsSum)]
                  )
                  ( TxOutDatumInline BabbageEraOnwardsConway
                      . makeHashableScriptData
                      . RedemptionSupplyDatum
                        redemptionsTreeRoot
                        (GenesisTimestamp genesisTimestampPlutusPOSIX)
                        redemptionIncrementPeriod
                      . Subdividing
                      $ redemptionsTreeDepth
                  )
                  ReferenceScriptNone
                  : if treasuryTokens == 0
                    then []
                    else
                      TxOut
                        ( AddressInEra
                            (ShelleyAddressInEra ShelleyBasedEraConway)
                            ( makeShelleyAddress
                                networkId
                                (PaymentCredentialByScript treasuryScriptHash)
                                stakeAddress
                            )
                        )
                        (valueToTxOutValue $ fromList $ [(starAssetId, Quantity treasuryTokens)])
                        TxOutDatumNone
                        ReferenceScriptNone
                        : ( ( \(address, star) ->
                                TxOut
                                  (AddressInEra (ShelleyAddressInEra ShelleyBasedEraConway) address)
                                  (valueToTxOutValue $ fromList $ [(starAssetId, Quantity star)])
                                  TxOutDatumNone
                                  ReferenceScriptNone
                            )
                              <$> foundationWalletsAdjusted
                          )
          , txProtocolParams = BuildTxWith (Just protocolParams)
          , txMintValue =
              mkTxMintValue
                MaryEraOnwardsConway
                [
                  ( PolicyId nightScriptHash
                  , PolicyAssets $
                      fromList
                        [(hydraThreadToken, -1)]
                  , BuildTxWith $
                      PlutusScriptWitness
                        PlutusScriptV3InConway
                        PlutusScriptV3
                        (PReferenceScript nightScriptTxIn)
                        NoScriptDatumForMint
                        ( makeHashableScriptData
                            . NightBurnGenesis
                            $ NightGenesis
                              (toPlutusScriptHash treasuryScriptHash)
                              genesisTimestampPlutusPOSIX
                              redemptionIncrementPeriod
                        )
                        (ExecutionUnits 0 0)
                  )
                ]
          , txExtraKeyWits =
              TxExtraKeyWitnesses AlonzoEraOnwardsConway (Foldable.toList tgeAgentAuthKeys)
          }
      )
      (AddressInEra (ShelleyAddressInEra ShelleyBasedEraConway) tgeWallet)
      Nothing
