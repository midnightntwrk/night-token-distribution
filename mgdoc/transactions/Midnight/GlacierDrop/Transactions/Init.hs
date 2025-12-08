module Midnight.GlacierDrop.Transactions.Init where

import Cardano.Api (
  Address,
  AddressInEra (AddressInEra),
  AddressTypeInEra (ShelleyAddressInEra),
  AlonzoEraOnwards (AlonzoEraOnwardsConway),
  AssetId (AssetId),
  AssetName (AssetName),
  BabbageEraOnwards (BabbageEraOnwardsConway),
  BuildTxWith (BuildTxWith),
  ConwayEra,
  EraHistory,
  ExecutionUnits (ExecutionUnits),
  Hash,
  KeyWitnessInCtx (KeyWitnessForSpending),
  MaryEraOnwards (MaryEraOnwardsConway),
  NetworkId,
  PaymentCredential (PaymentCredentialByScript),
  PaymentKey,
  PlutusScriptV3,
  PlutusScriptVersion (PlutusScriptV3),
  PolicyAssets (..),
  PolicyId (PolicyId),
  Quantity (Quantity),
  Script (PlutusScript, SimpleScript),
  ScriptDatum (NoScriptDatumForMint),
  ScriptHash,
  ScriptInAnyLang (ScriptInAnyLang),
  ScriptLanguage (PlutusScriptLanguage),
  ScriptLanguageInEra (PlutusScriptV3InConway),
  ScriptWitness (PlutusScriptWitness),
  ShelleyAddr,
  ShelleyBasedEra (ShelleyBasedEraConway),
  SimpleScript (RequireAnyOf),
  StakeAddressReference,
  SystemStart,
  TxBody,
  TxExtraKeyWitnesses (TxExtraKeyWitnesses),
  TxIn (TxIn),
  TxInsCollateral (TxInsCollateral),
  TxInsReference (TxInsReference),
  TxIx (TxIx),
  TxOut (TxOut),
  TxOutDatum (TxOutDatumInline, TxOutDatumNone),
  Witness (KeyWitness),
  calculateMinimumUTxO,
  defaultTxBodyContent,
  getTxBodyContent,
  getTxId,
  hashScript,
  lovelaceToValue,
  makeShelleyAddress,
  mkTxMintValue,
  toLedgerEpochInfo,
 )
import Cardano.Api.Shelley (
  LedgerProtocolParameters (unLedgerProtocolParameters),
  PlutusScript (PlutusScriptSerialised),
  PlutusScriptOrReferenceInput (PScript),
  ReferenceScript (ReferenceScript, ReferenceScriptNone),
  TxBodyContent (..),
  plutusScriptVersion,
 )
import Control.Monad (unless)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, mapStateT)
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Foldable (Foldable (toList))
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Midnight.GlacierDrop.Api (
  MerkleHash (MerkleHash),
  Sha512HalfScriptHash (Sha512HalfScriptHash),
  Star (Star),
 )
import Midnight.GlacierDrop.Scripts.Common (emptyTrieRoot)
import Midnight.GlacierDrop.Scripts.Common qualified as C
import Midnight.GlacierDrop.Scripts.Debug.HydraThreadCompile qualified as Debug.HydraThreadCompile
import Midnight.GlacierDrop.Scripts.Debug.NightCompile qualified as Debug.NightCompile
import Midnight.GlacierDrop.Scripts.Debug.RedemptionCompile qualified as Debug.RedemptionCompile
import Midnight.GlacierDrop.Scripts.Debug.RedemptionSupplyCompile qualified as Debug.RedemptionSupplyCompile
import Midnight.GlacierDrop.Scripts.HydraThread.Types (
  HydraThreadClaiming (
    MakeHydraThreadClaiming,
    allotmentsTreeDepth,
    allotmentsTreeRoot,
    alreadyClaimedTrieRoot,
    redemptionsSum
  ),
  HydraThreadDatum (HydraThreadClaiming, HydraThreadFinal),
  HydraThreadFinal (
    MakeHydraThreadFinal,
    finalTransactionId,
    redemptionsSum,
    redemptionsTreeDepth,
    redemptionsTreeRoot
  ),
  HydraThreadParams (
    HydraThreadParams,
    hydraCommitScriptHash,
    messagePrefix,
    messageSuffix,
    minAuthSignatures,
    sha512HalfScriptHash,
    tgeAgentAuthKeys,
    trieInsertCred
  ),
 )
import Midnight.GlacierDrop.Scripts.HydraThreadCompile qualified as HydraThreadCompile
import Midnight.GlacierDrop.Scripts.Night.Types (
  NightMintRedeemer (NightMintStar),
  NightParams (
    NightParams,
    allocationCount,
    jitterStrataCount,
    merkleRoot,
    minTreasuryTokens,
    protocolParamsCS,
    seedInput,
    supply,
    treeDepth
  ),
 )
import Midnight.GlacierDrop.Scripts.NightCompile qualified as NightCompile
import Midnight.GlacierDrop.Scripts.ProtocolParamsCompile (
  dynamicMintingLogicPlutusCredential,
  versionedProtocolParamsScript,
 )
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
  getAddressScriptHash,
  hashPlutusScript,
  hydraThreadToken,
  makeHashableScriptData,
  mkTxBody,
  scriptHashToPlutusCurrencySymbol,
  toPlutusAddress,
  toPlutusPubKeyHash,
  toPlutusScriptHash,
  toPlutusTxOutRef,
  valueToTxOutValue,
 )

import Cardano.Api.Shelley (toCtxUTxOTxOut)
import Convex.PlutusLedger.V1 (transPolicyId)
import Data.ByteString qualified as B
import GHC.IsList (IsList (fromList))
import Midnight.GlacierDrop.Contrib.Cardano.Api (
  AUTxO (..),
 )
import Midnight.GlacierDrop.Scripts.Debug.MerkleTrieFFICompile qualified as Debug.MerkleTrieFFICompile
import Midnight.GlacierDrop.Scripts.MerkleTrieFFICompile qualified as MerkleTrieFFICompile
import Midnight.GlacierDrop.Scripts.ProtocolParams.Types (
  NightMintingParams (..),
 )
import Midnight.GlacierDrop.Transactions.NightProtocolParams (
  InitNightProtocolParamsArgs (..),
  InitNightProtocolParamsError (..),
  buildInitNightProtocolParamsTransaction,
 )
import Numeric.Natural (Natural)
import PlutusLedgerApi.V3 qualified as P
import Refined (makeAscending, unAscending)

data MainError
  = TxBodyErrorAutoBalance Int (TxBodyErrorAutoBalanceAndCheckExUnits ConwayEra)
  | InitNightProtocolParamsError InitNightProtocolParamsError
  | InvalidInitArgs String
  | OtherError String
  deriving (Show)

data InitArgs = InitArgs
  { nightMintStarSeedUTxO :: AUTxO ConwayEra
  , allotmentsTreeRoot :: ByteString
  , allotmentsCount :: Natural
  , foundationWallets :: [(Address ShelleyAddr, Natural)]
  , minTreasuryTokens :: Natural
  , minAuthSignatures :: Natural
  , hydraCommitScriptHash :: ScriptHash
  , supply :: Natural
  , jitterStrataCount :: Natural
  , debug :: Bool
  , stakeAddress :: StakeAddressReference
  , redemptionShardDepth :: Natural
  , redemptionIncrements :: Natural
  , tgeAgentAuthKeys :: Set (Hash PaymentKey)
  , -- TODO: Currently we use this both to establish the NightParams & NightMintingParams, and as the required signers for the transaction.
    -- This means this value must contain all the multi-sig signers.
    -- If we want to allow minting to be performed by a subset of the signers greater than minAuthSignatures (as the contract allows)
    -- then we need to add an additional field for `required_signers`, that contains the subset of signers that will sign this transaction.
    tgeWallet :: Address ShelleyAddr
  , messagePrefix :: ByteString
  , messageSuffix :: ByteString
  , systemStart :: SystemStart
  , eraHistory :: EraHistory
  , protocolParams :: LedgerProtocolParameters ConwayEra
  , networkId :: NetworkId
  , nightProtocolParamsSeedUTxO :: AUTxO ConwayEra
  , nightProtocolParamsReserveScriptAddress :: P.Address
  }

data InitResult = InitResult
  { nightProtocolParamsTx :: TxBody ConwayEra
  , nightProtocolParams :: NightMintingParams
  , mintTx :: TxBody ConwayEra
  , publishNightTx :: TxBody ConwayEra
  , nightScript :: PlutusScript PlutusScriptV3
  , nightScriptReferenceOutput :: TxIn
  , publishRedemptionSupplyTx :: TxBody ConwayEra
  , redemptionSupplyScript :: PlutusScript PlutusScriptV3
  , redemptionSupplyScriptReferenceOutput :: TxIn
  , publishRedemptionTx :: TxBody ConwayEra
  , redemptionScript :: PlutusScript PlutusScriptV3
  , redemptionScriptReferenceOutput :: TxIn
  , publishHydraThreadTx :: TxBody ConwayEra
  , hydraThreadScript :: PlutusScript PlutusScriptV3
  , hydraThreadScriptReferenceOutput :: TxIn
  , publishNightProtocolParamsTx :: TxBody ConwayEra
  , nightProtocolParamsScript :: PlutusScript PlutusScriptV3
  , nightProtocolParamsScriptReferenceOutput :: TxIn
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

-- TODO: Incorporate some of those asserts into the flow - they were originally
-- placed in the plutus-scripts compilation module and executed during
-- params application. After separation of some params to the datum we should
-- execute them here.
--
-- import Data.List (nub, sortOn)
--   ( NightParams
--       { allocationSupplyScriptHash
--       , claimScriptHash
--       , redemptionSupplyScriptHash
--       , merkleRoot
--       , tgeAgentAuthKeys
--       , foundationWallets
--       , minTreasuryTokens
--       , minAuthSignatures
--       , supply
--       , allocationCount
--       , jitterStrataCount
--       }
--     ) =
--     assert (allocationCount <= supply)
--       . assert (0 < supply)
--       . assert (0 < allocationCount)
--       . assert (0 < jitterStrataCount)
--       . assert (1 < minAuthSignatures)
--       . assert
--         (minAuthSignatures < (toInteger $ length $ unAscending $ tgeAgentAuthKeys))
--       . assert (all ((> 0) . snd) foundationWallets)
--       . assert (length (nub $ fst <$> foundationWallets) == length foundationWallets)
--       . assert (sortOn fst foundationWallets == foundationWallets)
--       . assert (0 <= minTreasuryTokens)
--       . assert (coerce lengthOfByteString merkleRoot == (32 :: Integer))
--       . assert (checkHashLength hydraThreadScriptHash)
--       . assert (checkHashLength allocationSupplyScriptHash)
--       . assert (checkHashLength claimScriptHash)
--       . assert (checkHashLength redemptionSupplyScriptHash)
--       . assert (all checkHashLength $ unAscending tgeAgentAuthKeys)

buildInitTransaction :: InitArgs -> Either MainError InitResult
buildInitTransaction args = do
  let InitArgs
        { nightMintStarSeedUTxO
        , allotmentsTreeRoot
        , allotmentsCount
        , foundationWallets
        , minTreasuryTokens
        , minAuthSignatures
        , hydraCommitScriptHash
        , supply
        , jitterStrataCount
        , debug
        , stakeAddress
        , redemptionShardDepth
        , redemptionIncrements
        , tgeAgentAuthKeys
        , tgeWallet
        , messagePrefix
        , messageSuffix
        , systemStart
        , eraHistory
        , protocolParams
        , networkId
        , nightProtocolParamsSeedUTxO
        , nightProtocolParamsReserveScriptAddress
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

  -- It is not exactly circular but somewhat ;-)
  -- \* We want to build NightMintingParams which contains `supplyScriptHash` - night minting policy hash
  -- \* In order to get that hash we have to provide NightParams to the minting script
  -- \* But those params requrire protocol params policy hash
  -- \* The night params policy is derived from the seed txOutRef only
  -- \* We compute that hash in the first place, then the night script hash so we can wrap it in the minting params
  let nightProtocolParamsScriptSerialised =
        versionedProtocolParamsScript $
          toPlutusTxOutRef
            nightProtocolParamsSeedUTxO.aTxIn
      nightProtocolParamsScript = PlutusScript plutusScriptVersion nightProtocolParamsScriptSerialised
      nightProtocolParamsScriptHash = hashScript nightProtocolParamsScript

      redemptionSupply =
        if debug
          then Debug.RedemptionSupplyCompile.redemptionSupply
          else RedemptionSupplyCompile.redemptionSupply
      redemption =
        if debug
          then Debug.RedemptionCompile.redemption
          else RedemptionCompile.redemption
      hydraThread =
        if debug
          then Debug.HydraThreadCompile.hydraThread
          else HydraThreadCompile.hydraThread
      night =
        if debug
          then Debug.NightCompile.night
          else NightCompile.night
      insertTrieRewardingPlutusCredential =
        if debug
          then Debug.MerkleTrieFFICompile.insertTrieRewardingPlutusCredential
          else MerkleTrieFFICompile.insertTrieRewardingPlutusCredential

      redemptionSupplyScriptHash = hashPlutusScript redemptionSupplyScript
      redemptionSupplyScriptAddress =
        makeShelleyAddress
          networkId
          (PaymentCredentialByScript redemptionSupplyScriptHash)
          stakeAddress

      sha512HalfProxyScript =
        PlutusScriptSerialised
          . P.serialiseCompiledCode
          $ sha512HalfProxy

      sha512HalfProxyScriptHash = hashPlutusScript sha512HalfProxyScript

      hydraThreadScript =
        PlutusScriptSerialised
          . P.serialiseCompiledCode
          . hydraThread
          $ HydraThreadParams
            { hydraCommitScriptHash = toPlutusScriptHash hydraCommitScriptHash
            , messagePrefix = P.toBuiltin messagePrefix
            , messageSuffix = P.toBuiltin messageSuffix
            , tgeAgentAuthKeys =
                makeAscending $ toPlutusPubKeyHash <$> toList tgeAgentAuthKeys
            , minAuthSignatures = fromIntegral minAuthSignatures
            , sha512HalfScriptHash =
                Sha512HalfScriptHash $ toPlutusScriptHash sha512HalfProxyScriptHash
            , trieInsertCred = insertTrieRewardingPlutusCredential
            }
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
      sha512HalfProxy =
        if debug
          then Debug.HydraThreadCompile.sha512HalfProxy
          else HydraThreadCompile.sha512HalfProxy
      hydraThreadScriptHash = hashPlutusScript hydraThreadScript
      nightScript =
        PlutusScriptSerialised
          . P.serialiseCompiledCode
          . night
          $ NightParams
            { merkleRoot = MerkleHash (P.toBuiltin allotmentsTreeRoot)
            , treeDepth = ceiling $ logBase @Double 2 $ fromIntegral allotmentsCount
            , seedInput = toPlutusTxOutRef nightMintStarSeedUTxO.aTxIn
            , minTreasuryTokens = fromIntegral minTreasuryTokens
            , supply = fromIntegral supply
            , allocationCount = fromIntegral allotmentsCount
            , jitterStrataCount = fromIntegral jitterStrataCount
            , protocolParamsCS = transPolicyId (PolicyId nightProtocolParamsScriptHash)
            }
      nightScriptHash = hashPlutusScript nightScript
      nightScriptAddress =
        makeShelleyAddress
          networkId
          (PaymentCredentialByScript nightScriptHash)
          stakeAddress

      nightProtocolParams =
        NightMintingParams
          { dynamicMintingLogic =
              P.toBuiltinData $
                dynamicMintingLogicPlutusCredential $
                  scriptHashToPlutusCurrencySymbol nightProtocolParamsScriptHash
          , hydraThreadScriptHash = toPlutusScriptHash hydraThreadScriptHash
          , reserveScriptAddress = nightProtocolParamsReserveScriptAddress
          , supplyScriptAddress = toPlutusAddress redemptionSupplyScriptAddress
          , foundationWallets =
              bimap toPlutusAddress fromIntegral <$> foundationWallets
          , tgeAgentAuthKeys =
              unAscending $ makeAscending $ toPlutusPubKeyHash <$> toList tgeAgentAuthKeys
          , minAuthSignatures = fromIntegral minAuthSignatures
          }

  -- Optional TODO: This is not fully optimal:
  -- \* We publish the night protocol params later on:
  --    * The huge benefit of this is that we have a persistent storage
  --    for the smart contract bytecode.
  -- \* We don't use it in this transaction though
  -- \* We could fix it but it is not completely trivial:
  --    * We accept now two seed inputs
  --    * We do not accept any fee input per se
  --    * We should change that so we actually accept a single fee input which
  --      we subsequently use to fund **and** seed all the transactions
  --    * When we do this then we can have a publishing transaction which happens
  --      before mitning or publishing.
  --    * In the current setup it is impossible to achieve this
  nppTxBody <-
    first InitNightProtocolParamsError $
      buildInitNightProtocolParamsTransaction $
        InitNightProtocolParamsArgs
          { seedUTxO = nightProtocolParamsSeedUTxO
          , systemStart = systemStart
          , eraHistory = eraHistory
          , ledgerProtocolParams = protocolParams
          , nightProtocolParams
          , networkId
          }
  nppTxOut@(TxOut nppAddress _ _ _) <- case getTxBodyContent nppTxBody of
    TxBodyContent{txOuts = [out, _]} -> pure out
    _ ->
      Left $
        OtherError
          "Expected exactly one output in the night protocol params transaction body"

  nppScriptHash <-
    first
      (const $ OtherError "Expecting first output of nnp to be a script output")
      $ getAddressScriptHash nppAddress

  unless (nppScriptHash == nightProtocolParamsScriptHash) do
    Left $ OtherError "Incosistent protocol params script"

  let -- FIXME: This will be moved to a redeemer I believe at some point
      starAssetName = do
        let P.TokenName (P.fromBuiltin -> starTokenName) = C.starToken
        AssetName starTokenName
      assetId = AssetId (PolicyId nightScriptHash)
      starAssetId = assetId starAssetName
      supplyQuantity = coerce @(Natural -> Integer) fromIntegral supply
      calculateMinimumUTxO' =
        calculateMinimumUTxO
          ShelleyBasedEraConway
          (unLedgerProtocolParameters protocolParams)
      hydraThreadAddress =
        AddressInEra
          (ShelleyAddressInEra ShelleyBasedEraConway)
          ( makeShelleyAddress
              networkId
              (PaymentCredentialByScript hydraThreadScriptHash)
              stakeAddress
          )
      -- It is not sufficient to calculate min ada for the hydra thread output based on its datum
      -- but we need to consider the hydra thread utxo's maximum datum throughout its whole life in hydra.
      -- otherwise it might not have enough min ada when we want to move it back from hydra to l1.
      hydraThreadAda =
        max
          ( calculateMinimumUTxO' $
              TxOut
                hydraThreadAddress
                ( valueToTxOutValue $
                    fromList
                      [(assetId $ AssetName $ P.fromBuiltin $ P.unTokenName hydraThreadToken, 1)]
                )
                ( TxOutDatumInline BabbageEraOnwardsConway
                    . makeHashableScriptData
                    . HydraThreadClaiming
                    $ MakeHydraThreadClaiming
                      { allotmentsTreeRoot = MerkleHash (P.toBuiltin allotmentsTreeRoot)
                      , allotmentsTreeDepth =
                          ceiling $ logBase @Double 2 $ fromIntegral allotmentsCount
                      , alreadyClaimedTrieRoot = MerkleHash $ P.toBuiltin $ B.replicate 32 maxBound
                      , redemptionsSum = Star $ fromIntegral supply
                      }
                )
                ReferenceScriptNone
          )
          ( calculateMinimumUTxO' $
              TxOut
                hydraThreadAddress
                ( valueToTxOutValue $
                    fromList
                      [(assetId $ AssetName $ P.fromBuiltin $ P.unTokenName hydraThreadToken, 1)]
                )
                ( TxOutDatumInline BabbageEraOnwardsConway
                    . makeHashableScriptData
                    . HydraThreadFinal
                    $ MakeHydraThreadFinal
                      { finalTransactionId = P.TxId $ P.toBuiltin $ B.replicate 32 maxBound
                      , redemptionsTreeRoot = MerkleHash $ P.toBuiltin $ B.replicate 32 maxBound
                      , redemptionsTreeDepth = MerkleTreeDepth 255
                      , redemptionsSum = Star $ fromIntegral supply
                      }
                )
                ReferenceScriptNone
          )
          * 3
      nppTxId = getTxId nppTxBody
      nppTxIn = TxIn nppTxId (TxIx 0)

  flip
    evalStateT
    ( TxBuilderState
        nightMintStarSeedUTxO.aTxIn
        ( M.fromList
            [ (nightMintStarSeedUTxO.aTxIn, nightMintStarSeedUTxO.aTxOut)
            , (nppTxIn, toCtxUTxOTxOut nppTxOut)
            ]
        )
    )
    $ do
      TxBuilderState feeTxIn _ <- get
      mintTx <-
        mapStateT (first $ TxBodyErrorAutoBalance 0) $
          mkTxBody
            systemStart
            (toLedgerEpochInfo eraHistory)
            protocolParams
            (AddressInEra (ShelleyAddressInEra ShelleyBasedEraConway) tgeWallet)
            ( (defaultTxBodyContent ShelleyBasedEraConway)
                { txIns = [(feeTxIn, BuildTxWith $ KeyWitness KeyWitnessForSpending)]
                , txInsCollateral = TxInsCollateral AlonzoEraOnwardsConway [feeTxIn]
                , txInsReference =
                    TxInsReference
                      BabbageEraOnwardsConway
                      [nppTxIn]
                      (BuildTxWith mempty)
                , txOuts =
                    [ adjustForMinUTxO id protocolParams $
                        TxOut
                          (AddressInEra (ShelleyAddressInEra ShelleyBasedEraConway) nightScriptAddress)
                          ( valueToTxOutValue $ fromList [(starAssetId, supplyQuantity)]
                          )
                          (TxOutDatumInline BabbageEraOnwardsConway $ makeHashableScriptData ())
                          ReferenceScriptNone
                    , TxOut
                        ( AddressInEra
                            (ShelleyAddressInEra ShelleyBasedEraConway)
                            ( makeShelleyAddress
                                networkId
                                (PaymentCredentialByScript hydraThreadScriptHash)
                                stakeAddress
                            )
                        )
                        ( valueToTxOutValue $
                            fromList
                              [(assetId $ AssetName $ P.fromBuiltin $ P.unTokenName hydraThreadToken, 1)]
                              <> lovelaceToValue hydraThreadAda
                        )
                        ( TxOutDatumInline BabbageEraOnwardsConway
                            . makeHashableScriptData
                            . HydraThreadClaiming
                            $ MakeHydraThreadClaiming
                              { allotmentsTreeRoot = MerkleHash (P.toBuiltin allotmentsTreeRoot)
                              , allotmentsTreeDepth =
                                  ceiling $ logBase @Double 2 $ fromIntegral allotmentsCount
                              , alreadyClaimedTrieRoot = emptyTrieRoot
                              , redemptionsSum = 0
                              }
                        )
                        ReferenceScriptNone
                    ]
                , txMintValue =
                    mkTxMintValue
                      MaryEraOnwardsConway
                      [
                        ( PolicyId nightScriptHash
                        , PolicyAssets $
                            fromList
                              [ (starAssetName, supplyQuantity)
                              , (AssetName $ P.fromBuiltin $ P.unTokenName hydraThreadToken, 1)
                              ]
                        , BuildTxWith $
                            PlutusScriptWitness
                              PlutusScriptV3InConway
                              PlutusScriptV3
                              (PScript nightScript)
                              NoScriptDatumForMint
                              (makeHashableScriptData NightMintStar)
                              (ExecutionUnits 0 0)
                        )
                      ]
                , txExtraKeyWits =
                    TxExtraKeyWitnesses AlonzoEraOnwardsConway (toList tgeAgentAuthKeys)
                , txProtocolParams = BuildTxWith (Just protocolParams)
                }
            )
      publishNightTx <- mkPublishTx' 1 nightScript
      publishRedemptionSupplyTx <- mkPublishTx' 2 redemptionSupplyScript
      publishRedemptionTx <- mkPublishTx' 3 redemptionScript
      publishHydraThreadTx <- mkPublishTx' 4 hydraThreadScript
      publishNightProtocolParamsTx <-
        mkPublishTx' 5 nightProtocolParamsScriptSerialised
      pure $
        InitResult
          { nightProtocolParamsTx = nppTxBody
          , nightProtocolParams
          , mintTx
          , publishNightTx
          , nightScript
          , nightScriptReferenceOutput = TxIn (getTxId publishNightTx) (TxIx 0)
          , publishRedemptionSupplyTx
          , redemptionSupplyScript
          , redemptionSupplyScriptReferenceOutput =
              TxIn (getTxId publishRedemptionSupplyTx) (TxIx 0)
          , publishRedemptionTx
          , redemptionScript
          , redemptionScriptReferenceOutput =
              TxIn (getTxId publishRedemptionTx) (TxIx 0)
          , publishHydraThreadTx
          , hydraThreadScript
          , hydraThreadScriptReferenceOutput =
              TxIn (getTxId publishHydraThreadTx) (TxIx 0)
          , publishNightProtocolParamsTx
          , nightProtocolParamsScript = nightProtocolParamsScriptSerialised
          , nightProtocolParamsScriptReferenceOutput =
              TxIn (getTxId publishNightProtocolParamsTx) (TxIx 0)
          }
