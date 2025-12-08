{-# HLINT ignore "Use const" #-}
{-# LANGUAGE TemplateHaskell #-}
-- Needed for the below HLINT
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Midnight.GlacierDrop.NightApi where

import GHC.Generics (Generic)
import Midnight.GlacierDrop.Api (ScriptInfo (..))
import Midnight.GlacierDrop.Scripts.Common (
  trace,
  traceError,
 )
import PlutusLedgerApi.V3 (
  Address (Address),
  Credential (PubKeyCredential, ScriptCredential),
  CurrencySymbol,
  Map,
  POSIXTimeRange,
  PubKeyHash,
  Redeemer,
  ScriptHash,
  ScriptPurpose,
  StakingCredential,
  TokenName,
  TxInInfo (TxInInfo),
  TxOut (TxOut),
  TxOutRef,
  Value (getValue),
 )
import PlutusTx (makeLift)
import PlutusTx.AssocMap qualified as AMap
import PlutusTx.Builtins (
  chooseData,
  mkConstr,
  mkI,
  unsafeDataAsConstr,
 )
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Prelude hiding (trace, traceError)

type BuiltinTxInfoWithdrawals =
  BI.BuiltinList (BI.BuiltinPair BuiltinData BuiltinData)
type BuiltinCredential = BI.BuiltinData
type BuiltinTxInputs = BI.BuiltinList BuiltinData
type BuiltinTxOut = BuiltinData
type BuiltinTxInInfoFields = BI.BuiltinList BuiltinData

-- | A variant of 'PlutusLedgerApi.V3.NightScriptContext' that only includes the
-- data we need, uses domain types for the redeemer and datum, and moves
-- the redeemer into the 'ScriptInfo'
data NightScriptContext mintRedeemer spendRedeemer datum = NightScriptContext
  { scriptContextNightTxInfo :: NightTxInfo
  , scriptContextNightScriptInfo :: ScriptInfo mintRedeemer spendRedeemer datum
  }
  deriving stock (Generic)

-- | A version of PlutusLedgerApi.V3.NightTxInfo that only decodes what our
-- scripts need.
data NightTxInfo = NightTxInfo
  { txNightInfoInputs :: [TxInInfo]
  , txNightInfoReferenceInputs :: BuiltinData
  , txNightInfoOutputs :: [TxOut]
  , txNightInfoMint :: Value
  , txNightInfoWdrl :: BuiltinData
  , txNightInfoValidRange :: POSIXTimeRange
  , txNightInfoSignatories :: [PubKeyHash]
  , txNightInfoRedeemers :: Map ScriptPurpose Redeemer
  }
  deriving stock (Generic)

instance ToData NightTxInfo where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData NightTxInfo{..} =
    mkConstr
      0
      [ toBuiltinData txNightInfoInputs
      , txNightInfoReferenceInputs
      , toBuiltinData txNightInfoOutputs
      , mkI 0
      , toBuiltinData txNightInfoMint
      , mkI 0
      , txNightInfoWdrl
      , toBuiltinData txNightInfoValidRange
      , toBuiltinData txNightInfoSignatories
      , toBuiltinData txNightInfoRedeemers
      , mkI 0
      , mkI 0
      , mkI 0
      , mkI 0
      , mkI 0
      , mkI 0
      ]

instance UnsafeFromData NightTxInfo where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData d = case unsafeDataAsConstr d of
    ( _
      , (unsafeFromBuiltinData -> txNightInfoInputs)
          : txNightInfoReferenceInputs
          : (unsafeFromBuiltinData -> txNightInfoOutputs)
          : _
          : (unsafeFromBuiltinData -> txNightInfoMint)
          : _
          : txNightInfoWdrl
          : (unsafeFromBuiltinData -> txNightInfoValidRange)
          : (unsafeFromBuiltinData -> txNightInfoSignatories)
          : (unsafeFromBuiltinData -> txNightInfoRedeemers)
          : _
      ) -> NightTxInfo{..}
    _ -> error ()

instance FromData NightTxInfo where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData d =
    chooseData
      d
      ( \_ -> case unsafeDataAsConstr d of
          ( _
            , inputs
                : referenceInputs
                : outputs
                : _
                : txInfoMint
                : _
                : wdrl
                : validRange
                : signatories
                : txInfoRedeemers
                : _
            ) ->
              NightTxInfo
                <$> fromBuiltinData inputs
                <*> Just referenceInputs
                <*> fromBuiltinData outputs
                <*> fromBuiltinData txInfoMint
                <*> fromBuiltinData wdrl
                <*> fromBuiltinData validRange
                <*> fromBuiltinData signatories
                <*> fromBuiltinData txInfoRedeemers
          _ -> Nothing
      )
      (\_ -> Nothing)
      (\_ -> Nothing)
      (\_ -> Nothing)
      (\_ -> Nothing)
      ()

instance
  (ToData mintRedeemer, ToData spendRedeemer, ToData datum)
  => ToData (NightScriptContext mintRedeemer spendRedeemer datum)
  where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData NightScriptContext{..} =
    mkConstr
      0
      [ toBuiltinData scriptContextNightTxInfo
      , redeemerData
      , infoData
      ]
    where
      (redeemerData, infoData) = case scriptContextNightScriptInfo of
        MintingScript redeemer currencySymbol -> (toBuiltinData redeemer, mkConstr 0 [toBuiltinData currencySymbol])
        SpendingScript redeemer ownRef datum ->
          ( toBuiltinData redeemer
          , mkConstr 1 [toBuiltinData ownRef, toBuiltinData datum]
          )

instance
  ( UnsafeFromData mintRedeemer
  , UnsafeFromData spendRedeemer
  , UnsafeFromData datum
  )
  => UnsafeFromData (NightScriptContext mintRedeemer spendRedeemer datum)
  where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData d = case unsafeDataAsConstr d of
    ( _
      , [ unsafeFromBuiltinData -> scriptContextNightTxInfo
          , redeemerData
          , infoData
          ]
      ) -> NightScriptContext{..}
        where
          scriptContextNightScriptInfo = case unsafeDataAsConstr infoData of
            ( tag
              , [unsafeFromBuiltinData -> currencySymbol]
              )
                | tag == 0 ->
                    MintingScript (unsafeFromBuiltinData redeemerData) currencySymbol
            ( tag
              , [ unsafeFromBuiltinData -> ownRef
                  , unsafeFromBuiltinData -> datum
                  ]
              )
                | tag == 1 -> SpendingScript (unsafeFromBuiltinData redeemerData) ownRef datum
            _ -> error ()
    _ -> error ()

instance
  (FromData mintRedeemer, FromData spendRedeemer, FromData datum)
  => FromData (NightScriptContext mintRedeemer spendRedeemer datum)
  where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData d =
    chooseData
      d
      ( \_ -> case unsafeDataAsConstr d of
          ( _
            , [ fromBuiltinData -> Just scriptContextNightTxInfo
                , redeemerData
                , infoData
                ]
            ) -> NightScriptContext scriptContextNightTxInfo <$> scriptContextScriptInfo
              where
                scriptContextScriptInfo =
                  chooseData
                    infoData
                    ( \_ -> case unsafeDataAsConstr infoData of
                        ( tag
                          , [fromBuiltinData -> Just currencySymbol]
                          )
                            | tag == 0 ->
                                flip MintingScript currencySymbol <$> fromBuiltinData redeemerData
                        ( tag
                          , [ fromBuiltinData -> Just ownRef
                              , fromBuiltinData -> Just datum
                              ]
                          )
                            | tag == 1 ->
                                (\redeemer -> SpendingScript redeemer ownRef datum)
                                  <$> fromBuiltinData redeemerData
                        _ -> Nothing
                    )
                    (\_ -> Nothing)
                    (\_ -> Nothing)
                    (\_ -> Nothing)
                    (\_ -> Nothing)
                    ()
          _ -> Nothing
      )
      (\_ -> Nothing)
      (\_ -> Nothing)
      (\_ -> Nothing)
      (\_ -> Nothing)
      ()

makeLift ''NightTxInfo
makeLift ''NightScriptContext

-- | Performs standard extraction logic for CIP-69 multi-purpose scripts.
{-# INLINEABLE mkNightScript #-}
mkNightScript
  :: (NightTxInfo -> Map TokenName Integer -> CurrencySymbol -> mintRdmr -> Bool)
  -- ^ Callback that implements minting logic. It receives the tx info, the map
  -- of minted tokens, the currency symbol of the minted tokens, and the
  -- minting redeemer.
  -> ( NightTxInfo
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
  -> NightScriptContext mintRdmr spendRdmr datum
  -> Bool
mkNightScript mintLogic spendLogic (NightScriptContext txNightInfo scriptInfo) = case scriptInfo of
  MintingScript rdmr currency -> case AMap.toList $ getValue $ txNightInfoMint txNightInfo of
    [(symbol, mintedTokens)]
      | symbol == currency -> mintLogic txNightInfo mintedTokens currency rdmr
      | otherwise -> trace "Minted tokens missing" False
    _ -> trace "Wrong number of token mints" False
  SpendingScript rdmr ownRef mDatum -> case mDatum of
    Nothing -> trace "No input datum" False
    Just datum ->
      withSelfSeparatedNightTxInfo
        ownRef
        txNightInfo
        \( SelfSeparatedNightTxInfo
            txNightInfo'
            ownRef'
            scriptCredential
            stakingCredential
            inValue
          ) ->
            spendLogic
              txNightInfo'
              ownRef'
              scriptCredential
              stakingCredential
              inValue
              rdmr
              datum

data SelfSeparatedNightTxInfo = SelfSeparatedNightTxInfo_
  { txNightInfo :: NightTxInfo
  , ownRef :: TxOutRef
  , ownHash :: ScriptHash
  , stakingCredential :: Maybe StakingCredential
  , ownValue :: Value
  }

pattern SelfSeparatedNightTxInfo
  :: NightTxInfo
  -> TxOutRef
  -> ScriptHash
  -> Maybe StakingCredential
  -> Value
  -> SelfSeparatedNightTxInfo
pattern SelfSeparatedNightTxInfo txNightInfo ownRef ownHash stakingCredential ownValue =
  SelfSeparatedNightTxInfo_ txNightInfo ownRef ownHash stakingCredential ownValue

{-# COMPLETE SelfSeparatedNightTxInfo #-}

{-# INLINEABLE withSelfSeparatedNightTxInfo #-}
-- Given a `TxOutRef` (from `SpendingScript` purpose) and a `TxInfo` produce
-- a new `TxInfo` with the input removed and information about the input extracted.
withSelfSeparatedNightTxInfo
  :: TxOutRef -> NightTxInfo -> (SelfSeparatedNightTxInfo -> Bool) -> Bool
withSelfSeparatedNightTxInfo ownRef txNightInfo spendLogic = go id $ txNightInfoInputs txNightInfo
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
                let txNightInfo' = txNightInfo{txNightInfoInputs = f rest}
                spendLogic
                  $ SelfSeparatedNightTxInfo_
                    txNightInfo'
                    ownRef
                    scriptCredential
                    stakingCredential
                    inValue
            TxOut (Address (PubKeyCredential{}) _) _ _ _ ->
              traceError
                "Own input locked by `PubKeyCredential` instead of `ScriptCredential`"
      | otherwise = go (f . (TxInInfo ref txOut :)) rest

{-# INLINEABLE wrapNightScript #-}
wrapNightScript
  :: ( UnsafeFromData mintRedeemer
     , UnsafeFromData spendRedeemer
     , UnsafeFromData datum
     )
  => (params -> NightScriptContext mintRedeemer spendRedeemer datum -> Bool)
  -> params
  -> BuiltinData
  -> BI.BuiltinUnit
wrapNightScript script params context =
  check
    $ script
      params
      (unsafeFromBuiltinData context)
