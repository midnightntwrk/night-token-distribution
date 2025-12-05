{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}

module Midnight.GlacierDrop.Scripts.Common (
  ContBool,
  SelfSeparatedTxInfo (
    SelfSeparatedTxInfo,
    txInfo,
    ownRef,
    ownHash,
    stakingCredential,
    ownValue
  ),
  ScriptSignature,
  applyArg,
  applyParams,
  builtinAnd,
  checkHashLength,
  claimToken,
  deserializeDatum,
  diffTime,
  evaluateScript,
  isHydraThreadToken,
  getOwnInput,
  headSingleton,
  hydraThreadTokenPrefix,
  intersectionLength,
  isAdaOnly,
  isChangeOutput,
  mkScript,
  mkSpendingScript,
  starToken,
  starTokenBuiltin,
  trace,
  traceError,
  traceIfFalse,
  traceIfFalseBuiltin,
  withMaybeExtraToken,
  withMaybeExtraToken2,
  withContinuingOutputs,
  withOutputDatum,
  withSelfSeparatedTxInfo,
  wrapScript,
  hashNode,
  showDebug,
  withAnExtraToken,
  serializeDatum,
  emptyTrieRoot,
  newNightTokenName,
  toBurnTokenName,
  isToBurnToken,
) where

#ifdef TRACE_GHC
import qualified Debug.Trace as Debug
#else
import PlutusTx.Show (Show (show))
#endif
import PlutusLedgerApi.V1.Time (DiffMilliSeconds (..))
import PlutusLedgerApi.V3 hiding (
  ScriptContext (..),
  ScriptInfo (..),
  TxInfo (..),
 )
import PlutusTx.Builtins qualified as B
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Prelude qualified as P
import Test.QuickCheck.Instances ()

import Control.Exception (Exception (displayException), SomeException (..), try)
import Data.Coerce (Coercible, coerce)
import Data.Void (Void, absurd)
import GHC.ByteOrder (ByteOrder (BigEndian))
import GHC.IO (evaluate, unsafePerformIO)
import Midnight.GlacierDrop.Api (
  MerkleHash (MerkleHash),
  ScriptContext (ScriptContext),
  ScriptInfo (MintingScript, SpendingScript),
  TxInfo (txInfoInputs, txInfoMint),
 )
import PlutusCore (DefaultUni)
import PlutusLedgerApi.V1.Value (flattenValue, symbols)
import PlutusTx (
  CompiledCode,
  Lift,
  liftCodeDef,
  unsafeApplyCode,
 )
import PlutusTx.AssocMap qualified as AMap
import PlutusTx.Builtins.HasOpaque qualified as BI
import PlutusTx.Prelude hiding (trace, traceError, traceIfFalse)
import Refined (Ascending, unAscending)
import Prelude qualified as Haskell

#ifdef TRACE_GHC
trace :: Haskell.String -> a -> a
trace = Debug.trace
#else
trace :: BuiltinString -> a -> a
trace = P.trace
#endif

#ifdef TRACE_GHC
traceError :: Haskell.String -> a
traceError = Haskell.error
#else
traceError :: BuiltinString -> a
traceError = P.traceError
#endif

#ifdef TRACE_GHC
traceIfFalse :: Haskell.String -> Bool -> Bool
traceIfFalse s False = trace s False
traceIfFalse _ True = True
#else
traceIfFalse :: BuiltinString -> Bool -> Bool
traceIfFalse = P.traceIfFalse
#endif

#ifdef TRACE_GHC
showDebug :: (Haskell.Show a) => a -> Haskell.String
showDebug = Haskell.show
#else
showDebug :: (Show a) => a -> BuiltinString
showDebug = show
#endif

type ScriptSignature = BuiltinData -> BuiltinUnit

{-# INLINEABLE wrapScript #-}
wrapScript
  :: ( UnsafeFromData mintRedeemer
     , UnsafeFromData spendRedeemer
     , UnsafeFromData datum
     )
  => (params -> ScriptContext mintRedeemer spendRedeemer datum -> Bool)
  -> params
  -> ScriptSignature
wrapScript script params context =
  check
    $ script
      params
      (unsafeFromBuiltinData context)

applyArg
  :: (Lift DefaultUni params)
  => CompiledCode (params -> res)
  -> params
  -> CompiledCode res
applyArg code = unsafeApplyCode code . liftCodeDef

applyParams
  :: (Lift DefaultUni params)
  => CompiledCode (params -> ScriptSignature)
  -> params
  -> CompiledCode ScriptSignature
applyParams code = unsafeApplyCode code . liftCodeDef

evaluateScript
  :: (ToData mintRedeemer, ToData spendRedeemer, ToData datum)
  => (params -> ScriptSignature)
  -> params
  -> ScriptContext mintRedeemer spendRedeemer datum
  -> Haskell.Maybe Haskell.String
evaluateScript script params context =
  unsafePerformIO do
    result <-
      try
        $ evaluate
        $ script
          params
          (toBuiltinData context)
    case result of
      Haskell.Left (SomeException ex) -> Haskell.pure $ Haskell.Just $ displayException ex
      Haskell.Right _ -> Haskell.pure Nothing

type ContBool a = (a -> Bool) -> Bool

data SelfSeparatedTxInfo = SelfSeparatedTxInfo_
  { txInfo :: TxInfo
  , ownRef :: TxOutRef
  , ownHash :: ScriptHash
  , stakingCredential :: Maybe StakingCredential
  , ownValue :: Value
  }

pattern SelfSeparatedTxInfo
  :: TxInfo
  -> TxOutRef
  -> ScriptHash
  -> Maybe StakingCredential
  -> Value
  -> SelfSeparatedTxInfo
pattern SelfSeparatedTxInfo txInfo ownRef ownHash stakingCredential ownValue =
  SelfSeparatedTxInfo_ txInfo ownRef ownHash stakingCredential ownValue

{-# COMPLETE SelfSeparatedTxInfo #-}

{-# INLINEABLE withSelfSeparatedTxInfo #-}
-- Given a `TxOutRef` (from `SpendingScript` purpose) and a `TxInfo` produce
-- a new `TxInfo` with the input removed and information about the input extracted.
withSelfSeparatedTxInfo
  :: TxOutRef -> TxInfo -> (SelfSeparatedTxInfo -> Bool) -> Bool
withSelfSeparatedTxInfo ownRef txInfo spendLogic = go id $ txInfoInputs txInfo
  where
    go :: ([TxInInfo] -> [TxInInfo]) -> [TxInInfo] -> Bool
    go _ [] = traceError "Impossible. `txInfoInputs` should contain own `TxOutRef`."
    go f (TxInInfo ref txOut : rest)
      | ref == ownRef =
          case txOut of
            TxOut
              (Address (ScriptCredential scriptCredential) stakingCredential)
              inValue
              _
              _ -> do
                let txInfo' = txInfo{txInfoInputs = f rest}
                spendLogic
                  $ SelfSeparatedTxInfo_ txInfo' ownRef scriptCredential stakingCredential inValue
            TxOut (Address (PubKeyCredential{}) _) _ _ _ ->
              traceError
                "Own input locked by `PubKeyCredential` instead of `ScriptCredential`"
      | otherwise = go (f . (TxInInfo ref txOut :)) rest

-- | Performs standard extraction logic for CIP-69 multi-purpose scripts.
{-# INLINEABLE mkScript #-}
mkScript
  :: (TxInfo -> Map TokenName Integer -> CurrencySymbol -> mintRdmr -> Bool)
  -- ^ Callback that implements minting logic. It receives the tx info, the map
  -- of minted tokens, the currency symbol of the minted tokens, and the
  -- minting redeemer.
  -> ( TxInfo
       -> TxOutRef
       -> ScriptHash
       -> Maybe StakingCredential
       -> Value
       -> spendRdmr
       -> datum
       -> Bool
     )
  -- ^ Callback that implements spending logic. It receives a _modified_ tx
  -- info, the hash of the script, the value of the output being spent, the
  -- spending redeemer, and the datum of the output being spent.
  --
  -- The TxInfo's input will have the spent input removed to avoid needed to
  -- check for it multiple times in the script body.
  -> ScriptContext mintRdmr spendRdmr datum
  -> Bool
mkScript mintLogic spendLogic (ScriptContext txInfo scriptInfo) = case scriptInfo of
  MintingScript rdmr currency -> case AMap.toList $ getValue $ txInfoMint txInfo of
    [(symbol, mintedTokens)]
      | symbol == currency -> mintLogic txInfo mintedTokens currency rdmr
      | otherwise -> trace "Minted tokens missing" False
    _ -> trace "Wrong number of token mints" False
  SpendingScript rdmr ownRef mDatum -> case mDatum of
    Nothing -> trace "No input datum" False
    Just datum -> withSelfSeparatedTxInfo ownRef txInfo \(SelfSeparatedTxInfo txInfo' ownRef' scriptCredential stakingCredential inValue) ->
      spendLogic txInfo' ownRef' scriptCredential stakingCredential inValue rdmr datum

-- | Calls mkScript with a default minting action when minting is not
-- supported.
{-# INLINEABLE mkSpendingScript #-}
mkSpendingScript
  :: ( TxInfo
       -> TxOutRef
       -> ScriptHash
       -> Maybe StakingCredential
       -> Value
       -> spendRdmr
       -> datum
       -> Bool
     )
  -> ScriptContext Void spendRdmr datum
  -> Bool
mkSpendingScript = mkScript \_ _ _ -> absurd

-- Extract a list of outputs which are dedicated to own script address.

-- * We preserve the order of the inputs.

-- * We don't validate if the continuations exists - `[]` type is used for both).

-- * All the other outputs are passed unchecked.
{-# INLINEABLE withContinuingOutputs #-}
withContinuingOutputs
  :: (FromData datum)
  => ScriptHash
  -> Maybe StakingCredential
  -> [TxOut]
  -> ([(Value, datum)] -> [TxOut] -> Bool)
  -> Bool
withContinuingOutputs self stakingCredentail = go id id
  where
    go accOwn accOthers [] f = f (accOwn []) (accOthers [])
    go accOwn accOthers (txOut@(TxOut outaddress outValue outDatum _) : rest) f
      | outaddress == Address (ScriptCredential self) stakingCredentail =
          withOutputDatum outDatum \datum ->
            go (accOwn . ((outValue, datum) :)) accOthers rest f
      | otherwise = go accOwn (accOthers . (txOut :)) rest f

{-# INLINEABLE withOutputDatum #-}
withOutputDatum :: (FromData datum) => OutputDatum -> (datum -> Bool) -> Bool
withOutputDatum (OutputDatum (Datum d)) f = case fromBuiltinData d of
  Just datum -> f datum
  _ -> trace "Invalid output datum" False
withOutputDatum _ _ = trace "Non-inline output datum" False

{-# INLINEABLE diffTime #-}
diffTime :: POSIXTime -> POSIXTime -> DiffMilliSeconds
diffTime t0 t1 = DiffMilliSeconds $ getPOSIXTime $ t0 - t1

{-# INLINEABLE isChangeOutput #-}
isChangeOutput :: TxOut -> Bool
isChangeOutput = isAdaOnly . txOutValue

{-# INLINEABLE isAdaOnly #-}
isAdaOnly :: Value -> Bool
isAdaOnly = ([adaSymbol] ==) . symbols

{-# INLINEABLE intersectionLength #-}
intersectionLength :: (Ord a) => Ascending [a] -> Ascending [a] -> Integer
intersectionLength a b =
  go (unAscending a) (unAscending b) 0
  where
    go :: (Ord a) => [a] -> [a] -> Integer -> Integer
    go list0@(head0 : tail0) list1@(head1 : tail1) result =
      case head0 `compare` head1 of
        EQ -> go tail0 tail1 (result + 1)
        LT -> go tail0 list1 result
        GT -> go list0 tail1 result
    go _ _ result = result

{-# INLINEABLE starToken #-}
starToken :: TokenName
starToken = adaToken

{-# INLINEABLE newNightTokenName #-}
newNightTokenName :: TokenName
newNightTokenName = TokenName $ BI.stringToBuiltinByteStringUtf8 "NIGHT"

{-# INLINEABLE toBurnTokenName #-}
toBurnTokenName :: TokenName
toBurnTokenName = TokenName $ BI.stringToBuiltinByteStringUtf8 "ToBurn"

{-# INLINEABLE starTokenBuiltin #-}
starTokenBuiltin :: BI.BuiltinByteString
starTokenBuiltin = BI.emptyByteString

{-# INLINEABLE hydraThreadTokenPrefix #-}
hydraThreadTokenPrefix :: BuiltinByteString
hydraThreadTokenPrefix = "\1"

{-# INLINEABLE isHydraThreadToken #-}
isHydraThreadToken :: TokenName -> Bool
isHydraThreadToken tn = tn `isPrefixedWith` hydraThreadTokenPrefix

{-# INLINEABLE isToBurnToken #-}
isToBurnToken :: TokenName -> Bool
isToBurnToken tn = tn == toBurnTokenName

{-# INLINEABLE claimToken #-}
claimToken :: TokenName
claimToken = TokenName "\3"

{-# INLINEABLE isPrefixOf #-}
isPrefixOf :: BuiltinByteString -> BuiltinByteString -> Bool
isPrefixOf prefix src =
  let prefixLength = lengthOfByteString prefix
      prefix' = sliceByteString 0 prefixLength src
   in prefix' == prefix

{-# INLINEABLE isPrefixedWith #-}
isPrefixedWith :: TokenName -> BuiltinByteString -> Bool
isPrefixedWith (TokenName token) prefix =
  prefix `isPrefixOf` token

{-# INLINEABLE deserializeDatum #-}
deserializeDatum :: (FromData a) => OutputDatum -> ContBool a
deserializeDatum (OutputDatum (Datum a)) continue
  | Just b <- fromBuiltinData a = continue b
  | otherwise = trace "invalid datum" False
deserializeDatum _ _ = trace "non inline datum" False

serializeDatum :: (ToData a) => a -> OutputDatum
serializeDatum = OutputDatum . Datum . toBuiltinData

{-# INLINEABLE checkHashLength #-}
checkHashLength :: (Coercible BuiltinByteString a) => a -> Bool
checkHashLength = (== (28 :: Integer)) . coerce lengthOfByteString

-- | Given a UTxO value which has to contain ADA deconstruct it assuming that it
-- contains only a single extra token.
{-# INLINEABLE withAnExtraToken #-}
withAnExtraToken
  :: Value -> (Lovelace -> CurrencySymbol -> TokenName -> Integer -> Bool) -> Bool
withAnExtraToken value continue =
  case flattenValue value of
    [(s1, _, lovelace), (s2, t, q)] | s1 P.== adaSymbol -> continue (Lovelace lovelace) s2 t q
    [(s1, t, q), (s2, _, lovelace)] | s2 P.== adaSymbol -> continue (Lovelace lovelace) s1 t q
    _ -> trace "Invalid number of tokens" False

-- | Given a UTxO value which has to contain ADA deconstruct it assuming that it
-- contains only a single extra token.
{-# INLINEABLE withMaybeExtraToken #-}
withMaybeExtraToken
  :: Value
  -> (Lovelace -> Maybe (CurrencySymbol, TokenName, Integer) -> Bool)
  -> Bool
withMaybeExtraToken value continue =
  case flattenValue value of
    [(s1, _, lovelace), (s2, t, q)] | s1 P.== adaSymbol -> continue (Lovelace lovelace) (Just (s2, t, q))
    [(s1, t, q), (s2, _, lovelace)] | s2 P.== adaSymbol -> continue (Lovelace lovelace) (Just (s1, t, q))
    [(s1, _, lovelace)] | s1 P.== adaSymbol -> continue (Lovelace lovelace) Nothing
    _ -> trace "Invalid number of tokens" False

-- | Given a UTxO value which has to contain ADA deconstruct it assuming that it
-- contains at most two extra tokens.
{-# INLINEABLE withMaybeExtraToken2 #-}
withMaybeExtraToken2
  :: Value
  -> ( Lovelace
       -> Maybe (CurrencySymbol, TokenName, Integer)
       -> Maybe (CurrencySymbol, TokenName, Integer)
       -> Bool
     )
  -> Bool
withMaybeExtraToken2 value continue =
  case flattenValue value of
    [(sAda, _, lovelace)] | sAda P.== adaSymbol -> continue (Lovelace lovelace) Nothing Nothing
    [(sAda, _, lovelace), (s1, t1, q1)]
      | sAda P.== adaSymbol ->
          continue (Lovelace lovelace) (Just (s1, t1, q1)) Nothing
    [(s1, t1, q1), (sAda, _, lovelace)]
      | sAda P.== adaSymbol ->
          continue (Lovelace lovelace) (Just (s1, t1, q1)) Nothing
    [(sAda, _, lovelace), (s1, t1, q1), (s2, t2, q2)]
      | sAda P.== adaSymbol ->
          continue (Lovelace lovelace) (Just (s1, t1, q1)) (Just (s2, t2, q2))
    [(s1, t1, q1), (sAda, _, lovelace), (s2, t2, q2)]
      | sAda P.== adaSymbol ->
          continue (Lovelace lovelace) (Just (s1, t1, q1)) (Just (s2, t2, q2))
    [(s1, t1, q1), (s2, t2, q2), (sAda, _, lovelace)]
      | sAda P.== adaSymbol ->
          continue (Lovelace lovelace) (Just (s1, t1, q1)) (Just (s2, t2, q2))
    _ -> trace "Invalid number of tokens" False

{-# INLINEABLE hashNode #-}
hashNode :: MerkleHash -> Integer -> MerkleHash -> Integer -> MerkleHash
hashNode (MerkleHash leftBytes) leftAmount (MerkleHash rightBytes) rightAmount = do
  let leftAmountBytes = integerToByteString BigEndian 8 leftAmount
      rightAmountBytes = integerToByteString BigEndian 8 rightAmount
  MerkleHash
    . sha2_256
    $ leftBytes
    `appendByteString` leftAmountBytes
    `appendByteString` rightBytes
    `appendByteString` rightAmountBytes

emptyTrieRoot :: MerkleHash
emptyTrieRoot = MerkleHash $ B.replicateByte 32 0x00

instance Eq ScriptPurpose where
  Minting a == Minting b = a == b
  Spending a == Spending b = a == b
  Rewarding a == Rewarding b = a == b
  Certifying a0 a1 == Certifying b0 b1 = a0 == b0 && a1 == b1
  Voting a == Voting b = a == b
  Proposing a _ == Proposing b _ = a == b && traceError "to do"
  _ == _ = False

{-# INLINE traceIfFalseBuiltin #-}
traceIfFalseBuiltin :: BuiltinString -> BI.BuiltinBool -> BI.BuiltinBool
traceIfFalseBuiltin s b = BI.ifThenElse b (\_ -> b) (\_ -> BI.trace s b) BI.unitval

-- >>> headSingleton (BI.mkCons (BI.mkI 1) (BI.mkNilData BI.unitval))
-- I 1
-- >>> headSingleton (BI.mkCons (BI.mkI 1) (BI.mkCons (BI.mkI 2) (BI.mkNilData BI.unitval)))
-- PlutusTx.Builtins.Internal.error

-- | Returns the first element of a list, errors if there is more than one element.
{-# INLINEABLE headSingleton #-}
headSingleton :: forall a. BI.BuiltinList a -> a
headSingleton =
  B.caseList
    (\() -> traceError "List is empty.")
    ( \x xs ->
        B.caseList
          (\_ -> x)
          (\_ _ -> traceError "List contains more than one element.")
          xs
    )

{-# INLINEABLE getOwnInput #-}
getOwnInput
  :: BuiltinData -> BI.BuiltinList BuiltinData -> BI.BuiltinList BuiltinData
getOwnInput ownOutRef = go
  where
    go :: BI.BuiltinList BuiltinData -> BI.BuiltinList BuiltinData
    go !ins =
      let infoFields = BI.snd (BI.unsafeDataAsConstr $ BI.head ins)
          outRef = BI.head infoFields
       in BI.ifThenElse
            (BI.equalsData outRef ownOutRef)
            ( \_ ->
                infoFields
            )
            ( \_ ->
                go (BI.tail ins)
            )
            BI.unitval

{-# INLINE builtinAnd #-}
builtinAnd :: BI.BuiltinBool -> BI.BuiltinBool -> BI.BuiltinBool
builtinAnd b1 b2 = BI.ifThenElse b1 b2 BI.false
