module Midnight.GlacierDrop.Transactions.RemintHydra where

import Cardano.Api (
  Address,
  AddressInEra (AddressInEra),
  AddressTypeInEra (ShelleyAddressInEra),
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
  ScriptDatum (NoScriptDatumForMint),
  ScriptInAnyLang (..),
  ScriptLanguageInEra (PlutusScriptV3InConway),
  ScriptWitness (PlutusScriptWitness),
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
    txProtocolParams
  ),
  TxBodyErrorAutoBalance,
  TxExtraKeyWitnesses (TxExtraKeyWitnesses),
  TxIn,
  TxInsCollateral (TxInsCollateral),
  TxInsReference (TxInsReference),
  TxOut (..),
  TxOutDatum (TxOutDatumInline),
  UTxO (UTxO),
  Witness (KeyWitness),
  calculateMinimumUTxO,
  defaultTxBodyContent,
  hashScript,
  lovelaceToValue,
  makeShelleyAddress,
  makeTransactionBodyAutoBalance,
  mkTxMintValue,
  toLedgerEpochInfo,
 )
import Cardano.Api.Shelley (
  LedgerProtocolParameters (unLedgerProtocolParameters),
  PlutusScriptOrReferenceInput (PReferenceScript),
  ReferenceScript (..),
 )
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Foldable (Foldable (toList))
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import GHC.IsList (IsList (fromList))
import Midnight.GlacierDrop.Api (MerkleHash (MerkleHash), Star (Star))
import Midnight.GlacierDrop.Scripts.Common qualified as C
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
 )
import Midnight.GlacierDrop.Scripts.Night.Types (
  NightMintRedeemer (RemintHydraThread),
 )
import Midnight.GlacierDrop.Scripts.RedemptionSupply.Types (
  MerkleTreeDepth (MerkleTreeDepth),
 )
import Midnight.GlacierDrop.Transactions.Common (
  AddressScriptHashError,
  DeserializeDatumError,
  getTxBody,
  makeHashableScriptData,
  mkHydraThreadToken,
  valueToTxOutValue,
 )
import Numeric.Natural (Natural)
import PlutusLedgerApi.V3 qualified as P

data RemintHydraError
  = TxBodyErrorAutoBalance (TxBodyErrorAutoBalance ConwayEra)
  | AddressScriptHashError AddressScriptHashError
  | DeserializeDatumError DeserializeDatumError
  | MissingReferenceScript String
  | NightProtocolParamsInvalidDatum (TxOutDatum CtxUTxO ConwayEra)
  | WrongHydraState
  deriving (Show)

data RemintHydraArgs = RemintHydraArgs
  { nightScriptTxIn :: TxIn
  , nightScriptInput :: TxOut CtxUTxO ConwayEra
  , hydraThreadScriptTxIn :: TxIn
  , hydraThreadScriptInput :: TxOut CtxUTxO ConwayEra
  , nightParamsTxIn :: TxIn
  , nightParamsInput :: TxOut CtxUTxO ConwayEra
  , stakeAddress :: StakeAddressReference
  , newHydraThreadTokenSuffix :: ByteString
  , allotmentsTreeRoot :: ByteString
  , allotmentsCount :: Natural
  , supply :: Natural
  , feeTxIn :: TxIn
  , feeInput :: TxOut CtxUTxO ConwayEra
  , tgeAgentAuthKeys :: Set (Hash PaymentKey)
  , tgeWallet :: Address ShelleyAddr
  , systemStart :: SystemStart
  , eraHistory :: EraHistory
  , protocolParams :: LedgerProtocolParameters ConwayEra
  , networkId :: NetworkId
  }

buildRemintHydraTransaction
  :: RemintHydraArgs -> Either RemintHydraError (TxBody ConwayEra)
buildRemintHydraTransaction args = do
  let RemintHydraArgs
        { nightScriptTxIn
        , nightScriptInput
        , hydraThreadScriptTxIn
        , hydraThreadScriptInput
        , nightParamsTxIn
        , nightParamsInput
        , stakeAddress
        , newHydraThreadTokenSuffix
        , allotmentsTreeRoot
        , allotmentsCount
        , supply
        , feeTxIn
        , feeInput
        , tgeAgentAuthKeys
        , tgeWallet
        , systemStart
        , eraHistory
        , protocolParams
        , networkId
        } = args
      newHydraThreadTokenPrefixed =
        P.fromBuiltin $
          P.unTokenName $
            mkHydraThreadToken $
              P.toBuiltin newHydraThreadTokenSuffix
  nightScriptHash <-
    (\case (ScriptInAnyLang _ script) -> hashScript script)
      <$> case nightScriptInput of
        TxOut _ _ _ (ReferenceScript _ script) -> pure $ script
        TxOut _ _ _ ReferenceScriptNone -> Left $ MissingReferenceScript "missing night script"
  hydraThreadScriptHash <-
    (\case (ScriptInAnyLang _ script) -> hashScript script)
      <$> case hydraThreadScriptInput of
        TxOut _ _ _ (ReferenceScript _ script) -> pure $ script
        TxOut _ _ _ ReferenceScriptNone -> Left $ MissingReferenceScript "missing hydra thread script"
  let calculateMinimumUTxO' txOut =
        calculateMinimumUTxO
          ShelleyBasedEraConway
          (unLedgerProtocolParameters protocolParams)
          txOut
      hydraThreadAddress =
        AddressInEra
          (ShelleyAddressInEra ShelleyBasedEraConway)
          ( makeShelleyAddress
              networkId
              (PaymentCredentialByScript hydraThreadScriptHash)
              stakeAddress
          )
      -- it is not sufficient to calculate min ada for the hydra thread output based on its datum
      -- but we need to consider the hydra thread utxo's maximum datum throughout its whole life in hydra.
      -- otherwise it might not have enough min ada when we want to move it back from hydra to l1.
      hydraThreadAda =
        max
          ( calculateMinimumUTxO' $
              TxOut
                hydraThreadAddress
                ( valueToTxOutValue
                    . fromList
                    $ [ (AssetId (PolicyId nightScriptHash) (AssetName newHydraThreadTokenPrefixed), 1)
                      ]
                )
                ( TxOutDatumInline BabbageEraOnwardsConway
                    . makeHashableScriptData
                    . HydraThreadClaiming
                    $ MakeHydraThreadClaiming
                      { allotmentsTreeRoot = MerkleHash (P.toBuiltin allotmentsTreeRoot)
                      , allotmentsTreeDepth =
                          ceiling $ logBase @Double 2 $ fromIntegral $ allotmentsCount
                      , alreadyClaimedTrieRoot = MerkleHash $ P.toBuiltin $ B.replicate 32 maxBound
                      , redemptionsSum = Star $ fromIntegral $ supply
                      }
                )
                ReferenceScriptNone
          )
          ( calculateMinimumUTxO' $
              TxOut
                hydraThreadAddress
                ( valueToTxOutValue
                    . fromList
                    $ [ (AssetId (PolicyId nightScriptHash) (AssetName newHydraThreadTokenPrefixed), 1)
                      ]
                )
                ( TxOutDatumInline BabbageEraOnwardsConway
                    . makeHashableScriptData
                    . HydraThreadFinal
                    $ MakeHydraThreadFinal
                      { finalTransactionId = P.TxId $ P.toBuiltin $ B.replicate 32 maxBound
                      , redemptionsTreeRoot = MerkleHash $ P.toBuiltin $ B.replicate 32 maxBound
                      , redemptionsTreeDepth = MerkleTreeDepth 255
                      , redemptionsSum = Star $ fromIntegral $ supply
                      }
                )
                ReferenceScriptNone
          )
          * 3
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
          M.fromList
            [ (nightScriptTxIn, nightScriptInput)
            , (hydraThreadScriptTxIn, hydraThreadScriptInput)
            , (feeTxIn, feeInput)
            , (nightParamsTxIn, nightParamsInput)
            ]
      )
      ( (defaultTxBodyContent ShelleyBasedEraConway)
          { txIns =
              [ (feeTxIn, BuildTxWith $ KeyWitness KeyWitnessForSpending)
              ]
          , txInsCollateral = TxInsCollateral AlonzoEraOnwardsConway [feeTxIn]
          , txInsReference =
              TxInsReference
                BabbageEraOnwardsConway
                [nightScriptTxIn, hydraThreadScriptTxIn, nightParamsTxIn]
                (BuildTxWith S.empty)
          , txOuts =
              [ TxOut
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
                        [ (AssetId (PolicyId nightScriptHash) (AssetName newHydraThreadTokenPrefixed), 1)
                        ]
                        <> lovelaceToValue hydraThreadAda
                  )
                  ( TxOutDatumInline BabbageEraOnwardsConway
                      . makeHashableScriptData
                      . HydraThreadClaiming
                      $ MakeHydraThreadClaiming
                        { allotmentsTreeRoot = MerkleHash (P.toBuiltin allotmentsTreeRoot)
                        , allotmentsTreeDepth =
                            ceiling $ logBase @Double 2 $ fromIntegral $ allotmentsCount
                        , alreadyClaimedTrieRoot = C.emptyTrieRoot
                        , redemptionsSum = 0
                        }
                  )
                  ReferenceScriptNone
              ]
          , txProtocolParams = BuildTxWith (Just protocolParams)
          , txMintValue =
              mkTxMintValue
                MaryEraOnwardsConway
                [
                  ( PolicyId nightScriptHash
                  , PolicyAssets $ fromList [(AssetName newHydraThreadTokenPrefixed, 1)]
                  , BuildTxWith $
                      PlutusScriptWitness
                        PlutusScriptV3InConway
                        PlutusScriptV3
                        (PReferenceScript nightScriptTxIn)
                        NoScriptDatumForMint
                        ( makeHashableScriptData RemintHydraThread
                        )
                        (ExecutionUnits 0 0)
                  )
                ]
          , txExtraKeyWits =
              TxExtraKeyWitnesses AlonzoEraOnwardsConway (toList tgeAgentAuthKeys)
          }
      )
      (AddressInEra (ShelleyAddressInEra ShelleyBasedEraConway) tgeWallet)
      Nothing

starToken :: AssetName
starToken = AssetName $ P.fromBuiltin $ P.unTokenName C.starToken

claimToken :: AssetName
claimToken = AssetName $ P.fromBuiltin $ P.unTokenName C.claimToken
