module Midnight.GlacierDrop.Transactions.FakeRedemptionInit where

import Cardano.Api (
  Address,
  AddressInEra (AddressInEra),
  AddressTypeInEra (ShelleyAddressInEra),
  AllegraEraOnwards (..),
  AlonzoEraOnwards (AlonzoEraOnwardsConway),
  AssetId (..),
  AssetName (..),
  BabbageEraOnwards (BabbageEraOnwardsConway),
  BuildTxWith (BuildTxWith),
  ConwayEra,
  EraHistory,
  KeyWitnessInCtx (KeyWitnessForSpending),
  MaryEraOnwards (..),
  NetworkId,
  PaymentCredential (PaymentCredentialByScript),
  PlutusScriptV3,
  PlutusScriptVersion (PlutusScriptV3),
  PolicyAssets (PolicyAssets),
  PolicyId,
  Quantity (..),
  Script (PlutusScript, SimpleScript),
  ScriptInAnyLang (ScriptInAnyLang),
  ScriptLanguage (PlutusScriptLanguage),
  ScriptLanguageInEra (..),
  ScriptWitness (..),
  ShelleyAddr,
  ShelleyBasedEra (ShelleyBasedEraConway),
  SimpleScript (..),
  SlotNo,
  StakeAddressReference,
  SystemStart,
  TxBody,
  TxIn (TxIn),
  TxInsCollateral (TxInsCollateral),
  TxIx (TxIx),
  TxOut (TxOut),
  TxOutDatum (..),
  TxValidityLowerBound (..),
  Witness (KeyWitness),
  defaultTxBodyContent,
  getTxId,
  hashScript,
  makeShelleyAddress,
  mkTxMintValue,
  toLedgerEpochInfo,
 )
import Cardano.Api.Shelley (
  LedgerProtocolParameters,
  PlutusScript (PlutusScriptSerialised),
  TxBodyContent (..),
  scriptPolicyId,
 )
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, mapStateT)
import Data.Bifunctor (Bifunctor (first))
import Midnight.GlacierDrop.Scripts.Debug.RedemptionCompile qualified as Debug.RedemptionCompile
import Midnight.GlacierDrop.Scripts.Debug.RedemptionSupplyCompile qualified as Debug.RedemptionSupplyCompile
import Midnight.GlacierDrop.Scripts.RedemptionCompile qualified as RedemptionCompile
import Midnight.GlacierDrop.Scripts.RedemptionSupply.Types (
  MerkleTreeDepth (MerkleTreeDepth),
  RedemptionShardDepth (RedemptionShardDepth),
  RedemptionSupplyParams (..),
  TotalIncrements (..),
 )
import Midnight.GlacierDrop.Scripts.RedemptionSupplyCompile qualified as RedemptionSupplyCompile
import Midnight.GlacierDrop.Transactions.Common (
  TxBodyErrorAutoBalanceAndCheckExUnits,
  TxBuilderState (TxBuilderState),
  adjustForMinUTxO,
  clockPOSIXTimeToPlutusPOSIXTime,
  hashPlutusScript,
  makeHashableScriptData,
  mkTxBody,
  toPlutusAddress,
  toPlutusScriptHash,
  valueToTxOutValue,
 )

import Cardano.Api.Internal.Script (
  ReferenceScript (..),
  SimpleScriptOrReferenceInput (..),
 )
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as M
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.IsList (IsList (fromList))
import Midnight.GlacierDrop.Api (MerkleHash (..))
import Midnight.GlacierDrop.Contrib.Cardano.Api (AUTxO (..))
import Midnight.GlacierDrop.Scripts.RedemptionSupply.Types qualified as S
import Midnight.GlacierDrop.Scripts.ThawingSchedule qualified as TS
import Midnight.GlacierDrop.Transactions.NightProtocolParams (
  InitNightProtocolParamsError (..),
 )
import Numeric.Natural (Natural)
import PlutusLedgerApi.V3 qualified as P

data MainError
  = TxBodyErrorAutoBalance Int (TxBodyErrorAutoBalanceAndCheckExUnits ConwayEra)
  | InitNightProtocolParamsError InitNightProtocolParamsError
  | InvalidInitArgs String
  | OtherError String
  deriving (Show)

data RedemptionInitArgs = RedemptionInitArgs
  { txFeeUTxO :: AUTxO ConwayEra
  , jitterStrataCount :: Natural
  , debug :: Bool
  , stakeAddress :: StakeAddressReference
  , redemptionShardDepth :: Natural
  , redemptionIncrements :: Natural
  , tgeWallet :: Address ShelleyAddr
  , systemStart :: SystemStart
  , eraHistory :: EraHistory
  , protocolParams :: LedgerProtocolParameters ConwayEra
  , networkId :: NetworkId
  , redemptionTreeRootHash :: ByteString
  , redemptionTreeRootAmount :: Natural
  , redemptionTreeDepth :: Natural
  , redemptionIncrementPeriod :: TS.RedemptionIncrementPeriod
  , genesisTime :: POSIXTime
  , mintScript :: SimpleScript
  , slot :: SlotNo
  }

data RedemptionInitResult = RedemptionInitResult
  { publishRedemptionSupplyTx :: TxBody ConwayEra
  , redemptionSupplyScript :: PlutusScript PlutusScriptV3
  , redemptionSupplyScriptReferenceOutput :: TxIn
  , publishRedemptionTx :: TxBody ConwayEra
  , redemptionScript :: PlutusScript PlutusScriptV3
  , redemptionScriptReferenceOutput :: TxIn
  , initRedemptionSupplyTx :: TxBody ConwayEra
  , nightPolicyId :: PolicyId
  }

mkPublishTx
  :: LedgerProtocolParameters ConwayEra
  -> NetworkId
  -> SystemStart
  -> EraHistory
  -> StakeAddressReference
  -> Address ShelleyAddr
  -> Int
  -> PlutusScript PlutusScriptV3
  -> StateT
      TxBuilderState
      (Either MainError)
      (TxBody ConwayEra)
mkPublishTx protocolParams networkId systemStart eraHistory stakeAddress tgeWallet i script = do
  let constFalse = hashScript . SimpleScript . RequireAnyOf $ []
      referenceOutput =
        adjustForMinUTxO id protocolParams $
          TxOut
            ( AddressInEra
                (ShelleyAddressInEra ShelleyBasedEraConway)
                ( makeShelleyAddress
                    networkId
                    (PaymentCredentialByScript constFalse)
                    stakeAddress
                )
            )
            (valueToTxOutValue mempty)
            TxOutDatumNone
            ( ReferenceScript BabbageEraOnwardsConway
                . ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV3)
                . PlutusScript PlutusScriptV3
                $ script
            )
  TxBuilderState feeTxIn _ <- get
  mapStateT (first $ TxBodyErrorAutoBalance i) $
    mkTxBody
      systemStart
      (toLedgerEpochInfo eraHistory)
      protocolParams
      (AddressInEra (ShelleyAddressInEra ShelleyBasedEraConway) tgeWallet)
      ( (defaultTxBodyContent ShelleyBasedEraConway)
          { txIns = [(feeTxIn, BuildTxWith . KeyWitness $ KeyWitnessForSpending)]
          , txInsCollateral = TxInsCollateral AlonzoEraOnwardsConway [feeTxIn]
          , txOuts = [referenceOutput]
          , txProtocolParams = BuildTxWith (Just protocolParams)
          }
      )

mkInitRedemptionSupplyTx
  :: LedgerProtocolParameters ConwayEra
  -> NetworkId
  -> SystemStart
  -> EraHistory
  -> StakeAddressReference
  -> Address ShelleyAddr
  -> PlutusScript PlutusScriptV3
  -> ByteString
  -> Natural
  -> Natural
  -> TS.RedemptionIncrementPeriod
  -> POSIXTime
  -> SimpleScript
  -> SlotNo
  -> StateT TxBuilderState (Either MainError) (TxBody ConwayEra)
mkInitRedemptionSupplyTx
  protocolParams
  networkId
  systemStart
  eraHistory
  stakeAddress
  tgeWallet
  redemptionSupplyScript
  redemptionTreeRootHash
  redemptionTreeDepth
  redemptionTreeRootAmount
  redemptionIncrementPeriod
  genesisTime
  mintScript
  slot = do
    let supplyScriptHash = hashPlutusScript redemptionSupplyScript
        supplyAddr =
          AddressInEra
            (ShelleyAddressInEra ShelleyBasedEraConway)
            ( makeShelleyAddress
                networkId
                (PaymentCredentialByScript supplyScriptHash)
                stakeAddress
            )

    let datum =
          S.RedemptionSupplyDatum
            (MerkleHash (P.toBuiltin redemptionTreeRootHash))
            (TS.GenesisTimestamp (clockPOSIXTimeToPlutusPOSIXTime genesisTime))
            redemptionIncrementPeriod
            (S.Subdividing $ S.MerkleTreeDepth (fromIntegral redemptionTreeDepth))
        inlineDatum = TxOutDatumInline BabbageEraOnwardsConway (makeHashableScriptData datum)

    let mintScriptPolicyId = scriptPolicyId $ SimpleScript mintScript
        assetId = AssetId mintScriptPolicyId (AssetName "")
        mintQty = Quantity (fromIntegral redemptionTreeRootAmount)

    let baseVal = fromList [(AdaAssetId, 20_000_000)]
        mintVal = fromList [(assetId, mintQty)]
        totalVal = baseVal <> mintVal
        supplyOut =
          adjustForMinUTxO id protocolParams $
            TxOut supplyAddr (valueToTxOutValue totalVal) inlineDatum ReferenceScriptNone

    TxBuilderState feeTxIn _ <- get
    mapStateT (first $ TxBodyErrorAutoBalance 5) $
      mkTxBody
        systemStart
        (toLedgerEpochInfo eraHistory)
        protocolParams
        (AddressInEra (ShelleyAddressInEra ShelleyBasedEraConway) tgeWallet)
        ( (defaultTxBodyContent ShelleyBasedEraConway)
            { txIns = [(feeTxIn, BuildTxWith . KeyWitness $ KeyWitnessForSpending)]
            , txInsCollateral = TxInsCollateral AlonzoEraOnwardsConway [feeTxIn]
            , txOuts = [supplyOut]
            , txProtocolParams = BuildTxWith (Just protocolParams)
            , txMintValue =
                mkTxMintValue
                  MaryEraOnwardsConway
                  [
                    ( mintScriptPolicyId
                    , PolicyAssets $ fromList [(AssetName "", mintQty)]
                    , BuildTxWith $
                        SimpleScriptWitness
                          SimpleScriptInConway
                          (SScript mintScript)
                    )
                  ]
            , txValidityLowerBound = TxValidityLowerBound AllegraEraOnwardsConway (slot + 1)
            }
        )

buildRedemptionInitTransaction
  :: RedemptionInitArgs -> Either MainError RedemptionInitResult
buildRedemptionInitTransaction args = do
  let RedemptionInitArgs
        { txFeeUTxO
        , jitterStrataCount
        , debug
        , stakeAddress
        , redemptionShardDepth
        , redemptionIncrements
        , tgeWallet
        , systemStart
        , eraHistory
        , protocolParams
        , networkId
        , redemptionTreeRootHash
        , redemptionTreeRootAmount
        , redemptionTreeDepth
        , redemptionIncrementPeriod
        , genesisTime
        , mintScript
        , slot
        } =
          args
      mkPublishTx' =
        mkPublishTx
          protocolParams
          networkId
          systemStart
          eraHistory
          stakeAddress
          tgeWallet

      redemptionSupply =
        if debug
          then Debug.RedemptionSupplyCompile.redemptionSupply
          else RedemptionSupplyCompile.redemptionSupply
      redemption =
        if debug
          then Debug.RedemptionCompile.redemption
          else RedemptionCompile.redemption

      redemptionScript = PlutusScriptSerialised (P.serialiseCompiledCode redemption)
      redemptionScriptHash = hashPlutusScript redemptionScript
      redemptionSupplyScript =
        PlutusScriptSerialised
          . P.serialiseCompiledCode
          . redemptionSupply
          $ RedemptionSupplyParams
            { redemptionScriptHash = toPlutusScriptHash redemptionScriptHash
            , jitterStrataCount = fromIntegral jitterStrataCount
            , redemptionShardDepth =
                RedemptionShardDepth $ MerkleTreeDepth $ fromIntegral redemptionShardDepth
            , redemptionIncrements = TotalIncrements (fromIntegral redemptionIncrements)
            , tgeWalletAddress = toPlutusAddress tgeWallet
            }

  flip
    evalStateT
    ( TxBuilderState
        txFeeUTxO.aTxIn
        ( M.fromList
            [ (txFeeUTxO.aTxIn, txFeeUTxO.aTxOut)
            ]
        )
    )
    $ do
      TxBuilderState _ _ <- get
      publishRedemptionSupplyTx <- mkPublishTx' 2 redemptionSupplyScript
      publishRedemptionTx <- mkPublishTx' 3 redemptionScript
      initRedemptionSupplyTx <-
        mkInitRedemptionSupplyTx
          protocolParams
          networkId
          systemStart
          eraHistory
          stakeAddress
          tgeWallet
          redemptionSupplyScript
          redemptionTreeRootHash
          redemptionTreeDepth
          redemptionTreeRootAmount
          redemptionIncrementPeriod
          genesisTime
          mintScript
          slot

      pure $
        RedemptionInitResult
          { publishRedemptionSupplyTx
          , redemptionSupplyScript
          , redemptionSupplyScriptReferenceOutput =
              TxIn (getTxId publishRedemptionSupplyTx) (TxIx 0)
          , publishRedemptionTx
          , redemptionScript
          , redemptionScriptReferenceOutput =
              TxIn (getTxId publishRedemptionTx) (TxIx 0)
          , initRedemptionSupplyTx
          , nightPolicyId = scriptPolicyId $ SimpleScript mintScript
          }
