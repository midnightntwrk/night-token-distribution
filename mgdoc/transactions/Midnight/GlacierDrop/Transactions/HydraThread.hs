module Midnight.GlacierDrop.Transactions.HydraThread where

import Cardano.Api (
  AlonzoEraOnwards (AlonzoEraOnwardsConway),
  BabbageEraOnwards (BabbageEraOnwardsConway),
  BuildTx,
  BuildTxWith (BuildTxWith),
  ConwayEra,
  CtxUTxO,
  EraHistory (EraHistory),
  ExecutionUnits (ExecutionUnits),
  Hash,
  LedgerEpochInfo (unLedgerEpochInfo),
  PaymentKey,
  PlutusScript,
  PlutusScriptV3,
  PlutusScriptVersion (PlutusScriptV3),
  ScriptLanguageInEra (PlutusScriptV3InConway),
  ScriptWitness (PlutusScriptWitness),
  ShelleyBasedEra (ShelleyBasedEraConway),
  SystemStart (SystemStart),
  TxBody,
  TxBodyContent (
    txExtraKeyWits,
    txFee,
    txIns,
    txInsCollateral,
    txOuts,
    txProtocolParams,
    txWithdrawals
  ),
  TxBodyError,
  TxExtraKeyWitnesses (TxExtraKeyWitnesses),
  TxFee (..),
  TxIn (TxIn),
  TxInsCollateral (TxInsCollateral),
  TxOut (..),
  TxOutDatum (TxOutDatumInline),
  UTxO (UTxO),
  WitCtxStake,
  Witness (ScriptWitness),
  createTransactionBody,
  defaultTxBodyContent,
  makeSignedTransaction,
  toLedgerEpochInfo,
  toLedgerUTxO,
 )
import Cardano.Api.Shelley (
  LedgerProtocolParameters (unLedgerProtocolParameters),
  PlutusScriptOrReferenceInput (PScript),
  ReferenceScript (ReferenceScriptNone),
  ShelleyLedgerEra,
  Tx (ShelleyTx),
 )
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)

import Cardano.Api qualified as C
import Cardano.Api.Internal.Address qualified as C
import Cardano.Api.Ledger (StandardCrypto)
import Cardano.Ledger.Api (
  AlonzoEraScript (PlutusPurpose),
  AsIx,
  TransactionScriptFailure (ValidationFailure),
  evalTxExUnitsWithLogs,
 )
import Cardano.Ledger.Plutus (
  PlutusArgs (unPlutusV3Args),
  PlutusLanguage (PlutusArgs, isLanguage),
  PlutusWithContext (PlutusWithContext),
  SLanguage (SPlutusV1, SPlutusV2, SPlutusV3),
 )
import Cardano.Ledger.Slot (EpochSize (EpochSize))
import Data.Foldable (Foldable (toList))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.SOP.NonEmpty (NonEmpty (NonEmptyOne))
import Data.Set (Set)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Midnight.GlacierDrop.Api (MerkleHash (..))
import Midnight.GlacierDrop.Api qualified as A
import Midnight.GlacierDrop.Contrib.Cardano.Api (AUTxO (AUTxO))
import Midnight.GlacierDrop.Contrib.ShowCBORHex (ShowCBORHex (..))
import Midnight.GlacierDrop.Scripts.HydraThread.Terms (hashLeaf, mkLeaf)
import Midnight.GlacierDrop.Scripts.HydraThread.Terms qualified as H
import Midnight.GlacierDrop.Scripts.HydraThread.Types qualified as H
import Midnight.GlacierDrop.Scripts.MerkleTrieFFI.Types
import Midnight.GlacierDrop.Scripts.RedemptionSupply.Types (
  MerkleTreeDepth (MerkleTreeDepth),
 )
import Midnight.GlacierDrop.Scripts.Supply.Cardano.AddressBech32 qualified as S
import Midnight.GlacierDrop.Scripts.Supply.XRP qualified as S
import Midnight.GlacierDrop.Scripts.Supply.XRP.SHA512 qualified as S
import Midnight.GlacierDrop.Transactions.Common (
  AddressScriptHashError,
  DeserializeDatumError,
  deserializeDatumInline,
  hashPlutusScript,
  makeHashableScriptData,
  toPlutus,
 )
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Block (GenesisWindow (GenesisWindow))
import Ouroboros.Consensus.BlockchainTime (mkSlotLength)
import Ouroboros.Consensus.Cardano.Block (CardanoEras)
import Ouroboros.Consensus.HardFork.History (
  EraEnd (EraUnbounded),
  EraParams (
    EraParams,
    eraEpochSize,
    eraGenesisWin,
    eraSafeZone,
    eraSlotLength
  ),
  EraSummary (EraSummary, eraEnd, eraParams, eraStart),
  SafeZone (UnsafeIndefiniteSafeZone),
  Summary (Summary),
  initBound,
  mkInterpreter,
 )
import PlutusLedgerApi.V3 (ScriptContext)
import PlutusLedgerApi.V3 qualified as P
import PlutusTx qualified

data NightError
  = TxBodyError TxBodyError
  | AddressScriptHashError AddressScriptHashError
  | DeserializeDatumError DeserializeDatumError
  | WrongHydraState
  deriving (Show)

data ClaimMessageCfg = ClaimMessageCfg
  { messagePrefix :: ByteString
  , messageSuffix :: ByteString
  }
  deriving (Eq, Show)

newtype Sha512HalfProxyScript = Sha512HalfProxyScript (C.PlutusScript C.PlutusScriptV3)
  deriving (Show)

newtype InsertTrieScript = InsertTrieScript (C.PlutusScript C.PlutusScriptV3)
  deriving (Show)

data HydraClaimArgs = HydraClaimArgs
  { hydraTxIn :: TxIn
  , hydraInput :: TxOut CtxUTxO ConwayEra
  , hydraThreadScriptTxIn :: TxIn
  , claimInfo :: H.ClaimInfo
  , claimMessageCfg :: ClaimMessageCfg
  , insertTrieScript :: PlutusScript PlutusScriptV3
  , protocolParams :: LedgerProtocolParameters ConwayEra
  , feeUTxO :: AUTxO ConwayEra
  , hydraThreadScript :: PlutusScript PlutusScriptV3
  , sha512HalfProxyScript :: PlutusScript PlutusScriptV3
  , networkId :: C.NetworkId
  }

makeScriptWitnessWithScript
  :: forall witCtx redeemer
   . (P.ToData redeemer)
  => (C.IsScriptWitnessInCtx witCtx)
  => PlutusScript PlutusScriptV3
  -> redeemer
  -> C.ScriptDatum witCtx
  -> BuildTxWith BuildTx (Witness witCtx ConwayEra)
makeScriptWitnessWithScript script redeemer datum = do
  -- max tx/block budgets are specified in ./test/protoco-parameters-zero-fee.json
  -- All the scripts budgets should sum up to less than the max budget
  -- At the moment of writing those were:
  -- "memory":2000000000000000000,
  -- "steps": 2000000000000000000
  -- So we use here 1/4 of those values per script (we have 3 scripts in the transaction)
  -- We could switch to this strategy as well if we want to be more precise and
  -- waste some backend resources :-P import Convex.BuildTx (buildScriptWitness)
  let budget = 500000000000000000
  BuildTxWith
    . ScriptWitness C.scriptWitnessInCtx
    $ PlutusScriptWitness
      PlutusScriptV3InConway
      PlutusScriptV3
      (PScript script)
      datum
      (makeHashableScriptData redeemer)
      (ExecutionUnits budget budget)

mkMessage :: ClaimMessageCfg -> S.AddressBech32 -> A.Star -> A.PlainMessage
mkMessage messageCfg claimTo amount = do
  let messagePrefix = P.toBuiltin messageCfg.messagePrefix
      messageSuffix = P.toBuiltin messageCfg.messageSuffix
  H.mkMessage messagePrefix messageSuffix claimTo amount

mkSha512HalfWitness
  :: ClaimMessageCfg
  -> Sha512HalfProxyScript
  -> A.VerifiableClaimCredential
  -> S.AddressBech32
  -> A.Star
  -> C.NetworkId
  -> Maybe
      ( C.StakeAddress
      , C.Lovelace
      , BuildTxWith BuildTx (Witness WitCtxStake ConwayEra)
      )
mkSha512HalfWitness messageCfg (Sha512HalfProxyScript shaScript) claimFrom claimTo amount networkId = do
  let plainMessage = mkMessage messageCfg claimTo amount
      hashInfo = do
        pubKey <- case claimFrom of
          A.XrpCredential pk -> Just pk
          _ -> Nothing
        msg <- P.fromBuiltin <$> S.mkMessageHashPreimage pubKey plainMessage
        pure (P.toBuiltin msg, P.toBuiltin $ S.sha512HalfBS msg)
  case hashInfo of
    Nothing -> Nothing
    Just hi ->
      Just
        ( C.makeStakeAddress networkId $
            C.StakeCredentialByScript (hashPlutusScript shaScript)
        , mempty
        , makeScriptWitnessWithScript shaScript [hi] C.NoScriptDatumForStake
        )

mkTrieInsertWitness
  :: C.NetworkId
  -> InsertTrieScript
  -> MerkleHash
  -> MerkleHash
  -> A.VerifiableClaimCredential
  -> A.Star
  -> PlutusTx.BuiltinData -- alreadyClaimedTrieProof
  -> ( C.StakeAddress
     , C.Lovelace
     , BuildTxWith BuildTx (Witness WitCtxStake ConwayEra)
     )
mkTrieInsertWitness networkId (InsertTrieScript insertTrieScript) inputRoot outputRoot from amount proof =
  let insertTrieKey = getMerkleHash (hashLeaf $ mkLeaf from amount)
      insertTrieValue = ""
      insertTrieRedeemer =
        TrieInsertRedeemer
          { outputRoot = MerklePatriciaForestry (getMerkleHash outputRoot)
          , inputRoot = MerklePatriciaForestry (getMerkleHash inputRoot)
          , inputKey = insertTrieKey
          , inputValue = insertTrieValue
          , inputProof = case PlutusTx.fromBuiltinData proof of
              Just proof' -> proof'
              Nothing -> error $ "Failed to parse proof " <> show proof
          }
   in -- `do` syntax breaks on the tuple alone :-D
      -- so we need this `in`.

      ( C.makeStakeAddress networkId $
          C.StakeCredentialByScript (hashPlutusScript insertTrieScript)
      , C.quantityToLovelace $ C.Quantity 0
      , makeScriptWitnessWithScript
          insertTrieScript
          insertTrieRedeemer
          C.NoScriptDatumForStake
      )

mkTxWithdrawals
  :: ClaimMessageCfg
  -> Sha512HalfProxyScript
  -> InsertTrieScript
  -> A.VerifiableClaimCredential -- from
  -> S.AddressBech32 -- to
  -> A.Star -- amount
  -> C.NetworkId
  -> MerkleHash -- inputRoot
  -> MerkleHash -- outputRoot
  -> PlutusTx.BuiltinData -- alreadyClaimedTrieProof
  -> C.TxWithdrawals BuildTx ConwayEra
mkTxWithdrawals messageCfg shaScript insertTrieScript from to amount networkId inputRoot outputRoot proof = do
  let sha512Witness = mkSha512HalfWitness messageCfg shaScript from to amount networkId
      trieWitness =
        mkTrieInsertWitness
          networkId
          insertTrieScript
          inputRoot
          outputRoot
          from
          amount
          proof
      witnesses = trieWitness : foldMap pure sha512Witness
  C.TxWithdrawals C.shelleyBasedEra witnesses

buildHydraClaimTransaction
  :: HydraClaimArgs
  -> Either
      NightError
      ( TxBody ConwayEra
      , Map
          (PlutusPurpose AsIx (ShelleyLedgerEra ConwayEra))
          (ShowCBORHex ScriptContext)
      )
buildHydraClaimTransaction args = do
  let HydraClaimArgs
        { hydraTxIn
        , hydraInput
        , claimInfo =
          claimInfo@( H.ClaimInfo
                        { amount = claimAmount
                        , from = claimFrom
                        , to = claimTo
                        , signature = _
                        , allotmentsIndex = _
                        , allotmentsTreeProof = _
                        , alreadyClaimedTrieProof
                        , alreadyClaimedTrieRootNew
                        }
                      )
        , claimMessageCfg
        , insertTrieScript
        , protocolParams
        , feeUTxO = (AUTxO feeTxIn feeInput)
        , hydraThreadScript
        , networkId
        , sha512HalfProxyScript
        } = args
      TxOut hydraAddress hydraValue hydraDatum _ = hydraInput
  H.MakeHydraThreadClaiming
    { allotmentsTreeRoot
    , allotmentsTreeDepth
    , alreadyClaimedTrieRoot
    , redemptionsSum
    } <-
    (=<<) (\case (H.HydraThreadClaiming a) -> Right a; _ -> Left WrongHydraState)
      . (first DeserializeDatumError . deserializeDatumInline)
      $ hydraDatum

  txBody <-
    first TxBodyError $
      createTransactionBody
        ShelleyBasedEraConway
        ( (defaultTxBodyContent ShelleyBasedEraConway)
            { txFee = TxFeeExplicit ShelleyBasedEraConway 0
            , txIns =
                [
                  ( hydraTxIn
                  , makeScriptWitnessWithScript
                      hydraThreadScript
                      (H.HydraThreadClaim claimInfo)
                      C.InlineScriptDatum
                  )
                ]
            , txInsCollateral = TxInsCollateral AlonzoEraOnwardsConway [feeTxIn]
            , txOuts =
                [ TxOut
                    hydraAddress
                    hydraValue
                    ( TxOutDatumInline BabbageEraOnwardsConway
                        . makeHashableScriptData
                        . H.HydraThreadClaiming
                        $ H.MakeHydraThreadClaiming
                          { allotmentsTreeRoot
                          , allotmentsTreeDepth
                          , alreadyClaimedTrieRoot = alreadyClaimedTrieRootNew
                          , redemptionsSum = redemptionsSum + claimAmount
                          }
                    )
                    ReferenceScriptNone
                ]
            , txProtocolParams = BuildTxWith (Just protocolParams)
            , txWithdrawals =
                mkTxWithdrawals
                  claimMessageCfg
                  (Sha512HalfProxyScript sha512HalfProxyScript)
                  (InsertTrieScript insertTrieScript)
                  claimFrom
                  claimTo
                  claimAmount
                  networkId
                  alreadyClaimedTrieRoot
                  alreadyClaimedTrieRootNew
                  alreadyClaimedTrieProof
            }
        )
  pure
    ( txBody
    , fmap ShowCBORHex $
        case makeSignedTransaction [] txBody of
          ShelleyTx _ tx ->
            M.mapMaybe
              ( \case
                  Left
                    (ValidationFailure _ _ _ (PlutusWithContext _ _ _ (pwcArgs :: PlutusArgs l) _ _)) ->
                      case isLanguage @l of
                        SPlutusV1 -> Nothing
                        SPlutusV2 -> Nothing
                        SPlutusV3 -> Just $ unPlutusV3Args pwcArgs
                  _ -> Nothing
              )
              ( evalTxExUnitsWithLogs
                  (unLedgerProtocolParameters protocolParams)
                  tx
                  ( toLedgerUTxO ShelleyBasedEraConway $
                      UTxO $
                        M.fromList
                          [(hydraTxIn, hydraInput), (feeTxIn, feeInput)]
                  )
                  (unLedgerEpochInfo . toLedgerEpochInfo $ eraHistoryWithoutHorizon)
                  systemStart
              )
    )

data HydraFinalizeArgs = HydraFinalizeArgs
  { hydraTxIn :: TxIn
  , hydraInput :: TxOut CtxUTxO ConwayEra
  , redemptionsTreeRoot :: ByteString
  , redemptionsCount :: Natural
  , tgeAgentAuthKeys :: Set (Hash PaymentKey)
  , protocolParams :: LedgerProtocolParameters ConwayEra
  , feeTxIn :: TxIn
  , hydraThreadScript :: PlutusScript PlutusScriptV3
  }

buildHydraFinalizeTransaction
  :: HydraFinalizeArgs -> Either NightError (TxBody ConwayEra)
buildHydraFinalizeTransaction args = do
  let HydraFinalizeArgs
        { hydraTxIn = hydraTxIn@(TxIn txId _)
        , hydraInput
        , redemptionsTreeRoot = A.MerkleHash . P.toBuiltin -> redemptionsTreeRoot
        , redemptionsCount
        , tgeAgentAuthKeys
        , protocolParams
        , feeTxIn
        , hydraThreadScript
        } = args
      TxOut hydraAddress hydraValue hydraDatum _ = hydraInput
      redemptionsTreeDepth =
        MerkleTreeDepth . ceiling . logBase @Double 2 . fromIntegral $ redemptionsCount
  H.MakeHydraThreadClaiming _ _ _ redemptionsSum <-
    (=<<) (\case (H.HydraThreadClaiming a) -> Right a; _ -> Left WrongHydraState)
      . (first DeserializeDatumError . deserializeDatumInline)
      $ hydraDatum
  first TxBodyError $
    createTransactionBody
      ShelleyBasedEraConway
      ( (defaultTxBodyContent ShelleyBasedEraConway)
          { txFee = TxFeeExplicit ShelleyBasedEraConway 0
          , txIns =
              [
                ( hydraTxIn
                , makeScriptWitnessWithScript
                    hydraThreadScript
                    (H.HydraThreadFinalize redemptionsTreeRoot redemptionsTreeDepth)
                    C.InlineScriptDatum
                )
              ]
          , txInsCollateral = TxInsCollateral AlonzoEraOnwardsConway [feeTxIn]
          , txOuts =
              [ TxOut
                  hydraAddress
                  hydraValue
                  ( TxOutDatumInline BabbageEraOnwardsConway
                      . makeHashableScriptData
                      . H.HydraThreadFinal
                      $ H.MakeHydraThreadFinal
                        (P.TxId . toPlutus $ txId)
                        redemptionsTreeRoot
                        redemptionsTreeDepth
                        redemptionsSum
                  )
                  ReferenceScriptNone
              ]
          , txExtraKeyWits =
              TxExtraKeyWitnesses AlonzoEraOnwardsConway (toList tgeAgentAuthKeys)
          , txProtocolParams = BuildTxWith (Just protocolParams)
          }
      )

-- copied from https://github.com/cardano-scaling/hydra/blob/2bb89cff7625874b6efec8d9b9cf725c9315ef0f/hydra-tx/src/Hydra/Ledger/Cardano/Evaluate.hs#L293
eraHistoryWithoutHorizon :: EraHistory
eraHistoryWithoutHorizon =
  EraHistory (mkInterpreter summary)
  where
    summary :: Summary (CardanoEras StandardCrypto)
    summary =
      Summary . NonEmptyOne $
        EraSummary
          { eraStart = initBound
          , eraEnd = EraUnbounded
          , eraParams
          }

    eraParams =
      EraParams
        { eraEpochSize = EpochSize 1
        , eraSlotLength = mkSlotLength 1
        , -- NOTE: unused if the 'eraEnd' is already defined, but would be used to
          -- extend the last era accordingly in the real cardano-node
          eraSafeZone = UnsafeIndefiniteSafeZone
        , eraGenesisWin = GenesisWindow 1
        }

-- copied from https://github.com/cardano-scaling/hydra/blob/2bb89cff7625874b6efec8d9b9cf725c9315ef0f/hydra-tx/src/Hydra/Ledger/Cardano/Evaluate.hs#L322
systemStart :: SystemStart
systemStart = SystemStart $ posixSecondsToUTCTime 0
