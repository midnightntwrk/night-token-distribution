{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Midnight.GlacierDrop.Transactions.Common where

import Cardano.Api (
  Address,
  AddressInEra (AddressInEra),
  AsType (AsScriptHash),
  BalancedTxBody (BalancedTxBody),
  BuildTx,
  BuildTxWith (BuildTxWith),
  ConwayEra,
  CtxTx,
  CtxUTxO,
  ExecutionUnits (ExecutionUnits, executionMemory, executionSteps),
  HasTypeProxy (proxyToAsType),
  Hash,
  HashableScriptData,
  LedgerEpochInfo (unLedgerEpochInfo),
  NetworkId,
  PaymentCredential (PaymentCredentialByKey, PaymentCredentialByScript),
  PaymentKey,
  PlutusScript,
  PlutusScriptV3,
  PlutusScriptVersion (PlutusScriptV3),
  Script (PlutusScript),
  ScriptDatum (InlineScriptDatum),
  ScriptHash,
  ScriptLanguageInEra (PlutusScriptV3InConway),
  ScriptWitness (PlutusScriptWitness),
  ScriptWitnessInCtx (ScriptWitnessForSpending),
  SerialiseAsRawBytes (deserialiseFromRawBytes, serialiseToRawBytes),
  SerialiseAsRawBytesError,
  ShelleyAddr,
  ShelleyBasedEra (ShelleyBasedEraConway),
  StakeAddressPointer (StakeAddressPointer),
  StakeAddressReference (
    NoStakeAddress,
    StakeAddressByPointer,
    StakeAddressByValue
  ),
  StakeCredential,
  SystemStart,
  TxBody,
  TxBodyContent (TxBodyContent, txIns, txOuts),
  TxBodyErrorAutoBalance,
  TxIn (TxIn),
  TxIx (TxIx),
  TxOut (TxOut),
  TxOutDatum (TxOutDatumHash, TxOutDatumInline, TxOutDatumNone),
  TxOutValue (TxOutValueShelleyBased),
  UTxO (UTxO),
  Value,
  WitCtxTxIn,
  Witness (ScriptWitness),
  calculateMinimumUTxO,
  createTransactionBody,
  getScriptData,
  getTxBodyContent,
  getTxId,
  hashScript,
  lovelaceToTxOutValue,
  makeShelleyAddress,
  makeSignedTransaction,
  makeTransactionBodyAutoBalance,
  toCtxUTxOTxOut,
  toLedgerUTxO,
  unsafeHashableScriptData,
 )
import Cardano.Api qualified as C
import Cardano.Api.Internal.Tx.Body (TxInsReferenceDatums)
import Cardano.Api.Ledger (Coin, Credential, KeyRole (DRepRole))
import Cardano.Api.Shelley (
  Address (ShelleyAddress),
  LedgerProtocolParameters (LedgerProtocolParameters),
  PlutusScriptOrReferenceInput (PReferenceScript),
  PoolId,
  ReferenceScript (ReferenceScriptNone),
  StakeCredential (StakeCredentialByKey, StakeCredentialByScript),
  Tx (ShelleyTx),
  fromPlutusData,
  fromShelleyPaymentCredential,
  fromShelleyStakeReference,
  toMaryValue,
  toPlutusData,
 )
import Cardano.Api.Shelley qualified as CS
import Cardano.Ledger.Api (
  AlonzoEraScript (PlutusPurpose),
  AsIx,
  TransactionScriptFailure (ValidationFailure),
  evalTxExUnitsWithLogs,
  ppMaxTxExUnitsL,
 )
import Cardano.Ledger.BaseTypes (certIxFromIntegral, txIxFromIntegral)
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Credential (Ptr (Ptr))
import Cardano.Ledger.Mary.Value (MaryValue (MaryValue), MultiAsset (..))
import Cardano.Ledger.Plutus (
  ExUnits (ExUnits),
  PlutusArgs (unPlutusV3Args),
  PlutusLanguage (PlutusArgs, isLanguage),
  PlutusWithContext (PlutusWithContext),
  SLanguage (SPlutusV1, SPlutusV2, SPlutusV3),
 )
import Control.Lens ((^.))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State.Strict (
  StateT (StateT, runStateT),
  get,
  mapStateT,
  put,
 )
import Data.Bifunctor (Bifunctor (first))
import Data.Coerce (coerce)
import Data.Data (Proxy (Proxy))
import Data.Either (fromRight)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.SOP.NonEmpty qualified as SOP
import Data.Semigroup (Sum (Sum))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time (NominalDiffTime, UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Clock.POSIX qualified as Clock
import GHC.IsList (IsList (toList))
import Midnight.GlacierDrop.Scripts.Common (hydraThreadTokenPrefix)
import Ouroboros.Consensus.HardFork.History qualified as H
import PlutusLedgerApi.V1.Time (DiffMilliSeconds (DiffMilliSeconds))
import PlutusLedgerApi.V3 (ScriptContext, fromData)
import PlutusLedgerApi.V3 qualified as P
import Unsafe.Coerce (unsafeCoerce)

makeHashableScriptData :: (P.ToData a) => a -> HashableScriptData
makeHashableScriptData = unsafeHashableScriptData . fromPlutusData . P.toData

getTxBody :: BalancedTxBody era -> TxBody era
getTxBody (BalancedTxBody _ a _ _) = a

adjustForMinUTxO
  :: (TxOut CtxTx ConwayEra -> TxOut CtxTx ConwayEra)
  -> LedgerProtocolParameters ConwayEra
  -> TxOut CtxTx ConwayEra
  -> TxOut CtxTx ConwayEra
adjustForMinUTxO modifyOutput (LedgerProtocolParameters pParams) (TxOut addr value datum refScript) =
  TxOut addr (withMinAda value minAda) datum refScript
  where
    -- TODO(jamie) check if the bug in cardano-api that caused this to fail if the input
    -- value didn't have at least half an ada is still present.
    out' = modifyOutput $ TxOut addr (withMinAda value 500_000) datum refScript
    minAda = calculateMinimumUTxO ShelleyBasedEraConway pParams out'

withMinAda :: TxOutValue ConwayEra -> Coin -> TxOutValue ConwayEra
withMinAda (TxOutValueShelleyBased ShelleyBasedEraConway (MaryValue coin multiAsset)) minAda =
  TxOutValueShelleyBased ShelleyBasedEraConway $
    MaryValue (max coin minAda) multiAsset

isAdaOnly :: TxOutValue ConwayEra -> Bool
isAdaOnly = \case
  TxOutValueShelleyBased _ (MaryValue _ (MultiAsset multiAsset)) -> M.null multiAsset

data AddressScriptHashError = ScriptAddressByron | AddressKeyHash
  deriving (Show)

getAddressScriptHash
  :: AddressInEra ConwayEra -> Either AddressScriptHashError ScriptHash
getAddressScriptHash = \case
  AddressInEra _ CS.ByronAddress{} -> Left ScriptAddressByron
  AddressInEra _ (ShelleyAddress _ payment _) -> case fromShelleyPaymentCredential payment of
    PaymentCredentialByKey{} -> Left AddressKeyHash
    PaymentCredentialByScript hash -> Right hash

toPlutusTxOutRef :: TxIn -> P.TxOutRef
toPlutusTxOutRef (TxIn txId (TxIx txIx)) =
  P.TxOutRef (P.TxId $ P.toBuiltin $ serialiseToRawBytes $ txId) (toInteger txIx)

toPlutusAddress :: Address ShelleyAddr -> P.Address
toPlutusAddress (ShelleyAddress _ payment stake) =
  P.Address
    (toPlutusCredential $ fromShelleyPaymentCredential payment)
    (toStakeAddress $ fromShelleyStakeReference stake)

toPlutusCredential :: PaymentCredential -> P.Credential
toPlutusCredential = \case
  PaymentCredentialByKey pkh ->
    P.PubKeyCredential $ P.PubKeyHash $ P.toBuiltin $ serialiseToRawBytes pkh
  PaymentCredentialByScript sh -> P.ScriptCredential $ toPlutusScriptHash sh

scriptHashToPlutusCurrencySymbol :: ScriptHash -> P.CurrencySymbol
scriptHashToPlutusCurrencySymbol = P.CurrencySymbol . P.toBuiltin . serialiseToRawBytes

toStakeAddress :: StakeAddressReference -> Maybe P.StakingCredential
toStakeAddress = \case
  NoStakeAddress -> Nothing
  StakeAddressByValue cred -> Just $ P.StakingHash $ toPlutusCredentialStake cred
  StakeAddressByPointer _ ->
    error "Transactions.Common.toStakeAddress: StakeAddressByPointer not supported"

toPlutusCredentialStake :: StakeCredential -> P.Credential
toPlutusCredentialStake = \case
  StakeCredentialByKey pkh ->
    P.PubKeyCredential $ P.PubKeyHash $ P.toBuiltin $ serialiseToRawBytes pkh
  StakeCredentialByScript sh -> P.ScriptCredential $ toPlutusScriptHash sh

toPlutusScriptHash :: ScriptHash -> P.ScriptHash
toPlutusScriptHash = P.ScriptHash . P.toBuiltin . serialiseToRawBytes

toPlutusPubKeyHash :: Hash PaymentKey -> P.PubKeyHash
toPlutusPubKeyHash = P.PubKeyHash . P.toBuiltin . serialiseToRawBytes

fromPlutusScriptHash
  :: P.ScriptHash -> Either SerialiseAsRawBytesError ScriptHash
fromPlutusScriptHash =
  deserialiseFromRawBytes AsScriptHash
    . P.fromBuiltin
    . P.getScriptHash

hashPlutusScript :: PlutusScript PlutusScriptV3 -> ScriptHash
hashPlutusScript = hashScript . PlutusScript PlutusScriptV3

nominalDiffTimeToDiffMilliseconds :: NominalDiffTime -> DiffMilliSeconds
nominalDiffTimeToDiffMilliseconds =
  DiffMilliSeconds
    . floor
    . (* 1000)
    . nominalDiffTimeToSeconds

data DeserializeDatumError = InvalidInputDatum | MissingInputDatum
  deriving (Show)
deserializeDatumInline
  :: (P.FromData datum) => TxOutDatum ctx era -> Either DeserializeDatumError datum
deserializeDatumInline =
  \case
    TxOutDatumInline _ hashableScriptData ->
      maybe (Left InvalidInputDatum) pure
        . fromData
        . toPlutusData
        . getScriptData
        $ hashableScriptData
    _ -> Left MissingInputDatum

valueToTxOutValue :: Value -> TxOutValue ConwayEra
valueToTxOutValue = TxOutValueShelleyBased ShelleyBasedEraConway . toMaryValue

data SingleTokenValue = SingleTokenValue
  { lovelace :: C.Quantity
  , tokenAssetId :: C.AssetId
  , tokenAmount :: Integer
  }

-- `Clock.POSIXTime` is expressed in seconds with Pico precision.
-- `Plutus.POSIXTime` is expressed in milliseconds as an Integer.
clockPOSIXTimeToPlutusPOSIXTime :: Clock.POSIXTime -> P.POSIXTime
clockPOSIXTimeToPlutusPOSIXTime = P.POSIXTime . floor . (* 1000) . nominalDiffTimeToSeconds

parseSingleTokenValue :: Value -> Maybe SingleTokenValue
parseSingleTokenValue value = case toList value of
  [(C.AdaAssetId, lovelace), (assetId, C.Quantity tokenAmount)] -> do
    pure $ SingleTokenValue lovelace assetId tokenAmount
  [(assetId, C.Quantity tokenAmount), (C.AdaAssetId, lovelace)] -> do
    pure $ SingleTokenValue lovelace assetId tokenAmount
  _ -> Nothing

fromTxOutDatumCtxUTxO :: TxOutDatum CtxUTxO era -> TxOutDatum b era
fromTxOutDatumCtxUTxO TxOutDatumNone = TxOutDatumNone
fromTxOutDatumCtxUTxO (TxOutDatumHash a b) = TxOutDatumHash a b
fromTxOutDatumCtxUTxO (TxOutDatumInline a b) = TxOutDatumInline a b

makeScriptWitness
  :: (P.ToData redeemer)
  => TxIn
  -> ScriptHash
  -> redeemer
  -> BuildTxWith BuildTx (Witness WitCtxTxIn ConwayEra)
makeScriptWitness scriptTxIn _scriptHash redeemer =
  BuildTxWith
    . ScriptWitness ScriptWitnessForSpending
    $ PlutusScriptWitness
      PlutusScriptV3InConway
      PlutusScriptV3
      (PReferenceScript scriptTxIn)
      InlineScriptDatum
      (makeHashableScriptData redeemer)
      (ExecutionUnits 0 0)

data TxBuilderState = TxBuilderState
  { feeInput :: TxIn -- to do. `feeTxIn`
  , utxo :: Map TxIn (TxOut CtxUTxO ConwayEra)
  }

data TxBodyErrorAutoBalanceAndCheckExUnits era
  = ExecutionUnitsTooBig
      ExecutionUnits
      -- ^ Max ExecutionUnits from the protocol parameters
      ExecutionUnits
      -- ^ ExecutionUnits supplied
  | TxBodyErrorAutoBalance (TxBodyErrorAutoBalance era)
  deriving (Show)

makeTransactionBodyAutoBalanceAndCheckExUnits
  :: ShelleyBasedEra ConwayEra
  -> SystemStart
  -> LedgerEpochInfo
  -> LedgerProtocolParameters ConwayEra
  -> Set PoolId
  -> Map StakeCredential Coin
  -> Map (Credential 'DRepRole) Coin
  -> UTxO ConwayEra
  -> TxBodyContent BuildTx ConwayEra
  -> AddressInEra ConwayEra
  -> Maybe Word
  -> Either
      (TxBodyErrorAutoBalanceAndCheckExUnits ConwayEra)
      (BalancedTxBody ConwayEra)
makeTransactionBodyAutoBalanceAndCheckExUnits
  sbe
  systemstart
  history
  lpp@(LedgerProtocolParameters pp) =
    (=<<)
      ( \transactionBodyAutoBalance@(BalancedTxBody (TxBodyContent{txIns}) _ _ _) ->
          ( \case
              (Sum executionSteps, Sum executionMemory)
                | executionSteps <= maxExecutionSteps && executionMemory <= maxExecutionMemory ->
                    pure transactionBodyAutoBalance
                | otherwise ->
                    Left $
                      ExecutionUnitsTooBig
                        (ExecutionUnits maxExecutionSteps maxExecutionMemory)
                        (ExecutionUnits executionSteps executionMemory)
          )
            . foldMap
              ( \case
                  ( _
                    , BuildTxWith
                        ( ScriptWitness
                            _
                            (PlutusScriptWitness _ _ _ _ _ (ExecutionUnits{executionSteps, executionMemory}))
                          )
                    ) -> (Sum executionSteps, Sum executionMemory)
                  _ -> mempty
              )
            $ txIns
      )
      .:::. first TxBodyErrorAutoBalance
      .:::. makeTransactionBodyAutoBalance sbe systemstart history lpp
    where
      ExUnits maxExecutionMemory maxExecutionSteps = pp ^. ppMaxTxExUnitsL

infixr 8 .:::.
(.:::.)
  :: (b -> c)
  -> (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b)
  -> a0
  -> a1
  -> a2
  -> a3
  -> a4
  -> a5
  -> a6
  -> c
(.:::.) = (.) . (.) . (.) . (.) . (.) . (.) . (.)

mkTxBody
  :: SystemStart
  -> LedgerEpochInfo
  -> LedgerProtocolParameters ConwayEra
  -> AddressInEra ConwayEra
  -> TxBodyContent BuildTx ConwayEra
  -> StateT
      TxBuilderState
      (Either (TxBodyErrorAutoBalanceAndCheckExUnits ConwayEra))
      (TxBody ConwayEra)
mkTxBody systemStart epochInfo protocolParams tgeWalletAddressInEra content = do
  TxBuilderState{utxo} <- get
  BalancedTxBody _ txBody _ _ <-
    lift $
      makeTransactionBodyAutoBalanceAndCheckExUnits
        ShelleyBasedEraConway
        systemStart
        epochInfo
        protocolParams
        mempty
        mempty
        mempty
        (UTxO utxo)
        content
        tgeWalletAddressInEra
        Nothing
  let mkTxIn = TxIn (getTxId txBody) . TxIx
  let inputs = M.fromList $ txIns $ getTxBodyContent $ txBody
  let outputs =
        M.fromList
          . zipWith ((,) . mkTxIn) [0 ..]
          . fmap toCtxUTxOTxOut
          . txOuts
          . getTxBodyContent
          $ txBody
  let utxo' = M.union outputs $ M.difference utxo inputs
  put $
    TxBuilderState
      { feeInput = mkTxIn $ fromIntegral $ M.size outputs - 1
      , utxo = utxo'
      }
  pure txBody

mkTxBodyDebug
  :: SystemStart
  -> LedgerEpochInfo
  -> LedgerProtocolParameters ConwayEra
  -> AddressInEra ConwayEra
  -> TxBodyContent BuildTx ConwayEra
  -> StateT
      TxBuilderState
      ( Either
          ( TxBodyErrorAutoBalanceAndCheckExUnits ConwayEra
          , Map (PlutusPurpose AsIx (CS.ShelleyLedgerEra ConwayEra)) ScriptContext
          )
      )
      (TxBody ConwayEra)
mkTxBodyDebug systemStart epochInfo protocolParams tgeWalletAddressInEra content = do
  TxBuilderState{utxo} <- get
  mapStateT
    ( first
        (,validationFailureScriptContexts
            ShelleyBasedEraConway
            systemStart
            epochInfo
            protocolParams
            (UTxO utxo)
            content
            tgeWalletAddressInEra)
    )
    (mkTxBody systemStart epochInfo protocolParams tgeWalletAddressInEra content)

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks i xs = case splitAt i xs of
  (chunk, xs') -> chunk : chunks i xs'

utcTimeToPOSIXTime :: UTCTime -> P.POSIXTime
utcTimeToPOSIXTime =
  P.POSIXTime
    . floor
    . (* 1000)
    . nominalDiffTimeToSeconds
    . utcTimeToPOSIXSeconds

toPlutus :: (SerialiseAsRawBytes a) => a -> P.BuiltinByteString
toPlutus = P.toBuiltin . serialiseToRawBytes

fromPlutus
  :: forall a
   . (SerialiseAsRawBytes a)
  => P.BuiltinByteString
  -> Either SerialiseAsRawBytesError a
fromPlutus = deserialiseFromRawBytes (proxyToAsType $ Proxy @a) . P.fromBuiltin

data FromPlutusAddressError
  = SerialiseAsRawBytesError SerialiseAsRawBytesError
  | TxIxOutOfRange String
  | CertIxOutOfRange String
  deriving (Show)

fromPlutusAddress
  :: P.Address -> NetworkId -> Either FromPlutusAddressError (Address ShelleyAddr)
fromPlutusAddress (P.Address credential stakingCredential) networkId =
  do
    makeShelleyAddress networkId
      <$> first
        SerialiseAsRawBytesError
        ( case credential of
            P.PubKeyCredential a -> fmap PaymentCredentialByKey $ fromPlutus $ P.getPubKeyHash $ a
            P.ScriptCredential a -> fmap PaymentCredentialByScript $ fromPlutusScriptHash $ a
        )
      <*> ( case stakingCredential of
              Nothing -> pure NoStakeAddress
              Just (P.StakingPtr slot transaction certificate) ->
                fmap StakeAddressByPointer
                  . fmap StakeAddressPointer
                  $ Ptr (fromInteger slot)
                    <$> ( first TxIxOutOfRange
                            . coerce @(HowToAvoidDefiningThisType L.TxIx)
                            . txIxFromIntegral
                            $ transaction
                        )
                    <*> ( first CertIxOutOfRange
                            . coerce @(HowToAvoidDefiningThisType L.CertIx)
                            . certIxFromIntegral
                            $ certificate
                        )
              Just (P.StakingHash (P.PubKeyCredential a)) ->
                fmap StakeAddressByValue
                  . fmap StakeCredentialByKey
                  . (first SerialiseAsRawBytesError . fromPlutus)
                  . P.getPubKeyHash
                  $ a
              Just (P.StakingHash (P.ScriptCredential a)) ->
                fmap StakeAddressByValue
                  . fmap StakeCredentialByScript
                  . (first SerialiseAsRawBytesError . fromPlutusScriptHash)
                  $ a
          )

validationFailureScriptContexts
  :: ShelleyBasedEra ConwayEra
  -> SystemStart
  -> LedgerEpochInfo
  -> LedgerProtocolParameters ConwayEra
  -> UTxO ConwayEra
  -> TxBodyContent BuildTx ConwayEra
  -> AddressInEra ConwayEra
  -> Map (PlutusPurpose AsIx (CS.ShelleyLedgerEra ConwayEra)) ScriptContext
validationFailureScriptContexts
  sbe
  systemstart
  history
  (LedgerProtocolParameters pp)
  utxo
  txbodycontent
  changeaddr =
    fromRight mempty
      . fmap
        ( \txbody ->
            case makeSignedTransaction [] txbody of
              ShelleyTx _ tx ->
                M.mapMaybe
                  ( \case
                      Left
                        (ValidationFailure _ _ _ (PlutusWithContext _ _ _ (pwcArgs :: PlutusArgs l) _ _)) ->
                          case isLanguage @l of
                            SPlutusV1 -> Nothing
                            SPlutusV2 -> Nothing
                            SPlutusV3 -> Just $ unPlutusV3Args $ pwcArgs
                      _ -> Nothing
                  )
                  ( evalTxExUnitsWithLogs
                      pp
                      tx
                      (toLedgerUTxO sbe utxo)
                      (unLedgerEpochInfo history)
                      systemstart
                  )
        )
      -- shamefully copied from https://cardano-api.cardano.intersectmbo.org/cardano-api/internal/src/Cardano.Api.Fees.html#makeTransactionBodyAutoBalance
      $ createTransactionBody
        sbe
        txbodycontent
          { txOuts =
              txOuts txbodycontent
                ++ [ TxOut changeaddr (lovelaceToTxOutValue sbe 0) TxOutDatumNone ReferenceScriptNone
                   ]
                   -- TODO: think about the size of the change output
                   -- 1,2,4 or 8 bytes?
          }

newtype HowToAvoidDefiningThisType a = HowToAvoidDefiningThisType (Either String a)
  deriving newtype (Functor, Applicative, Monad)

-- is the answer in https://github.com/haskell/core-libraries-committee/issues/28 ?

instance MonadFail HowToAvoidDefiningThisType where
  fail :: forall a. String -> HowToAvoidDefiningThisType a
  fail = coerce @(String -> Either String a) Left

catch
  :: StateT state (Either e) a
  -> (e -> StateT state (Either e) a)
  -> StateT state (Either e) a
m `catch` handler =
  StateT $
    \s ->
      case runStateT m s of
        Left e -> runStateT (handler e) s
        result -> result

maxSafeSlot :: C.EraHistory -> C.SlotNo
maxSafeSlot (C.EraHistory interpreter) =
  getMaxSafeSlotFromSummary $ unInterpreter interpreter
  where
    unInterpreter :: H.Interpreter xs -> H.Summary xs
    unInterpreter = unsafeCoerce

    getMaxSafeSlotFromSummary :: H.Summary xs -> C.SlotNo
    getMaxSafeSlotFromSummary (H.Summary eras) = case eras of
      SOP.NonEmptyOne era -> getMaxSafeSlotFromEraSummary era
      SOP.NonEmptyCons _ eras' -> getMaxSafeSlotFromSummary (H.Summary eras')

    getMaxSafeSlotFromEraSummary :: H.EraSummary -> C.SlotNo
    getMaxSafeSlotFromEraSummary H.EraSummary{..} = case eraEnd of
      H.EraEnd H.Bound{..} -> C.SlotNo $ C.unSlotNo boundSlot - 1 -- subtract 1 because the era end bound is exclusive
      H.EraUnbounded -> maxBound

scriptHashFromReferenceInput
  :: C.TxOut C.CtxUTxO C.ConwayEra -> Maybe C.ScriptHash
scriptHashFromReferenceInput (C.TxOut _ _ _ possibleScriptReference) = case possibleScriptReference of
  CS.ReferenceScript
    _
    (C.ScriptInAnyLang _ (CS.PlutusScript PlutusScriptV3 script)) -> Just $ hashPlutusScript script
  _ -> Nothing

hydraThreadToken :: P.TokenName
hydraThreadToken = mkHydraThreadToken "token"

mkHydraThreadToken :: P.BuiltinByteString -> P.TokenName
mkHydraThreadToken tn = P.TokenName $ hydraThreadTokenPrefix <> tn

txInsReferenceDatumsFromDatum
  :: forall ctx era. TxOutDatum ctx era -> Maybe (TxInsReferenceDatums BuildTx)
txInsReferenceDatumsFromDatum = \case
  TxOutDatumInline _ hashableScriptData -> pure $ BuildTxWith (Set.singleton hashableScriptData)
  TxOutDatumNone -> pure $ BuildTxWith Set.empty
  _ -> Nothing

mkAddressInConwayEra :: C.Address C.ShelleyAddr -> C.AddressInEra C.ConwayEra
mkAddressInConwayEra = C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraConway)

clockDriftCompensation :: NominalDiffTime
clockDriftCompensation = 2 -- seconds
