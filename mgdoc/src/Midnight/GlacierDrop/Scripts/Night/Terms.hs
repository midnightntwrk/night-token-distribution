{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}
module Midnight.GlacierDrop.Scripts.Night.Terms where

import Data.Bifunctor (Bifunctor (second))
import Midnight.GlacierDrop.Api (Star (..))
import Midnight.GlacierDrop.NightApi (
  BuiltinCredential,
  BuiltinTxInfoWithdrawals,
  BuiltinTxInputs,
  NightScriptContext (..),
  NightTxInfo (..),
  mkNightScript,
 )
import Midnight.GlacierDrop.Scripts.Common (
  ContBool,
  deserializeDatum,
  diffTime,
  emptyTrieRoot,
  intersectionLength,
  isAdaOnly,
  isChangeOutput,
  isHydraThreadToken,
  serializeDatum,
  showDebug,
  starToken,
  trace,
  traceIfFalse,
  withMaybeExtraToken,
 )

-- Because of the conflict between the `HydraThreadClaiming` type and the
-- Datum constructor we use a qualified import here.
import Midnight.GlacierDrop.Scripts.HydraThread.Types (HydraThreadDatum (..))

import Midnight.GlacierDrop.Scripts.HydraThread.Types qualified as HT
import Midnight.GlacierDrop.Scripts.Night.Types (
  NightGenesis (NightGenesis),
  NightMintRedeemer (..),
  NightParams (
    NightParams,
    merkleRoot,
    minTreasuryTokens,
    protocolParamsCS,
    seedInput,
    supply,
    treeDepth
  ),
 )
import Midnight.GlacierDrop.Scripts.ProtocolParams.Types (
  NightMintingParams (..),
 )
import Midnight.GlacierDrop.Scripts.RedemptionSupply.Types (
  RedemptionSupplyDatum (RedemptionSupplyDatum),
  RedemptionSupplyState (Subdividing),
 )
import Midnight.GlacierDrop.Scripts.ThawingSchedule (
  GenesisTimestamp (GenesisTimestamp),
  finiteTxValidityRange,
 )
import PlutusLedgerApi.V1.Time (DiffMilliSeconds (..), POSIXTime (..))
import PlutusLedgerApi.V3 (
  Address (Address, addressCredential),
  Credential (PubKeyCredential, ScriptCredential),
  CurrencySymbol (CurrencySymbol),
  Map,
  Redeemer (Redeemer),
  ScriptHash (ScriptHash),
  ScriptPurpose (Minting),
  StakingCredential,
  TokenName,
  TxInInfo (TxInInfo, txInInfoResolved),
  TxOut (TxOut),
  TxOutRef,
  Value (..),
 )
import PlutusPrelude (coerce, (&))
import PlutusTx qualified
import PlutusTx.AssocMap qualified as M
import PlutusTx.Builtins qualified as EBI
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Foldable (sum)
import PlutusTx.List (all, length, map, tail, zip)
import PlutusTx.Prelude hiding (error, trace, traceError, traceIfFalse)
import Refined (makeAscending, unsafeMakeAscending)

-- | Check if the withdrawal list contains a withdrawal for the dynamic logic credential.
-- If it does, return True, otherwise error.
{-# INLINEABLE checkWithdrawZero #-}
checkWithdrawZero
  :: BuiltinCredential -> BuiltinTxInfoWithdrawals -> BI.BuiltinBool
checkWithdrawZero dynamicLogicCred = go
  where
    go :: BuiltinTxInfoWithdrawals -> BI.BuiltinBool
    go !wdrls =
      let wdrlCredential = BI.fst $ BI.head wdrls
       in BI.ifThenElse
            (BI.equalsData wdrlCredential dynamicLogicCred)
            (\_ -> BI.true)
            (\_ -> go (BI.tail wdrls))
            BI.unitval

-- | Obtain the protocol parameters UTxO from the reference inputs and return it's datum.
-- If the protocol parameters UTxO is not found, error.
{-# INLINEABLE getDynamicParams #-}
getDynamicParams
  :: CurrencySymbol -> BuiltinTxInputs -> NightMintingParams
getDynamicParams (CurrencySymbol protocolParamsCS) = go
  where
    go :: BuiltinTxInputs -> NightMintingParams
    go !ins =
      -- `let currentInput = head ins`
      --     gets the current input (data-encoded)
      -- `let currentInputFields = BI.snd $ unsafeDataAsConstr currentInput`
      --     gets the field list of the current input
      -- `let currentInputResolvedFields = BI.snd $ BI.unsafeConstrData $ BI.head $ BI.tail currentInputFields`
      --    `currentInputFields` is the data-encoded fields of `TxInInfo`.
      --    the first field of `TxInInfo` is the `txInOutRef` which we skip with `BI.tail`
      --    the second field of `TxInInfo` is the `txInInfoResolved` which is a data-encoded `TxOut`, we get this with `BI.head`
      --    we then destructure the data-encoded `txInInfoResolved` to obtain the data-encoded `TxOut` fields using `BI.snd BI.unsafeConstrAsData`
      --  So `txInResolvedFields` is the fields of a data-encoded `TxOut` type.
      let txInResolvedFields =
            BI.snd
              $ BI.unsafeDataAsConstr
              $ BI.head
              $ BI.tail
              $ BI.snd (BI.unsafeDataAsConstr $ BI.head ins)
          -- The first field of `TxOut` is `txOutAddress` which we skip with `BI.tail`
          -- The second field of `TxOut` is `txOutValue` which we get with `BI.head`
          -- We use `BI.unsafeDataAsMap` to extract the inner map of the data-encoded `Value` type
          -- So `inputValue` is the inner map of a data-encoded `Value` type.
          inputValueSkipAda = BI.tail $ BI.unsafeDataAsMap $ BI.head $ BI.tail txInResolvedFields
       in -- The protocol params script enforces that the protocol params UTxO must have exactly two tokens, ada and the protocol parameters NFT.
          -- Since the ledger enforces that values are ordered lexicographically, the first token must be ada and the second token must be the protocol parameters NFT.
          EBI.caseList
            (\_ -> go (BI.tail ins))
            ( \csPair _ ->
                let firstNonAdaToken = BI.fst csPair
                 in BI.ifThenElse
                      (BI.equalsByteString (BI.unsafeDataAsB firstNonAdaToken) protocolParamsCS)
                      ( \_ ->
                          -- if the first non-ada token is the protocol params NFT, we return the night minting params
                          PlutusTx.unsafeFromBuiltinData @NightMintingParams
                            $ BI.head
                            $ BI.snd
                            $ BI.unsafeDataAsConstr
                            $ BI.head
                            $ BI.tail
                            $ BI.tail txInResolvedFields
                      )
                      -- if the first non-ada token is not the protocol params NFT, we continue to the next input
                      (\_ -> go (BI.tail ins))
                      BI.unitval
            )
            inputValueSkipAda

{-# INLINEABLE nightScript #-}
nightScript
  :: NightParams -> NightScriptContext NightMintRedeemer () () -> Bool
nightScript params = mkNightScript (nightMint params) (nightSpend params)

-- This branch executes when the script is invoked with as a MintingScript.
{-# INLINEABLE nightMint #-}
nightMint
  :: NightParams
  -> NightTxInfo
  -> Map TokenName Integer
  -> CurrencySymbol
  -> NightMintRedeemer
  -> Bool
nightMint
  params
  ( NightTxInfo
      { txNightInfoInputs
      , txNightInfoReferenceInputs
      , txNightInfoWdrl
      , txNightInfoOutputs
      , txNightInfoSignatories
      , txNightInfoValidRange
      }
    )
  mintedValue
  policyId
  redeemer =
    case redeemer of
      -- Inputs Validation:
      --  1. The transaction must spend exactly one input, `seedUTxO`.
      --    1.1. The txOutRef of `seedUTxO` must equal `params.seedInput`
      --    1.2. The value in `seedUTxO` must only contain ADA.
      -- Minted Value Validation:
      --  2. The minted value, `mintedValue`, must equal `Value.singleton ownPolicyId starToken supply <> Value.singleton ownPolicyId hydraThreadToken 1`
      -- Output Validation:
      --  3. The transaction must contain at-least two outputs, `nightOut` and `hydraOut`.
      --  3.1. Night Output Validation:
      --         Address Validation:
      --           3.1.1. Payment credential must be equal to `ScriptCredential ownScriptHash`
      --           3.1.2. Staking credential may be anything
      --         Value Validation:
      --           3.1.3. The value must contain only ADA and STAR tokens.
      --           3.1.4. `stripAdaValue $ txOutValue nightOut` must equal `Value.singleton ownPolicyId starToken supply`
      --         Datum Validation:
      --           3.1.5. The `nightOut` datum must be equal to `()` in BuiltinData representation.
      --  3.2. Hydra Thread Output Validation:
      --         Address Validation:
      --           3.2.1. Payment credential must be equal to `ScriptCredential dynamicParams.hydraThreadScriptHash`
      --           3.2.2. Staking credential may be anything
      --         Value Validation:
      --           3.2.3. The value must contain only ADA and hydra thread tokens.
      --           3.2.4. `stripAdaValue $ txOutValue hydraOut` must equal `Value.singleton ownPolicyId hydraThreadToken 1`
      --         Datum Validation:
      --           3.2.5. The `hydraOut` datum must be equal to `HydraThreadClaiming {
      --             allotmentsTreeRoot = params.merkleRoot
      --             allotmentsTreeDepth = params.treeDepth
      --               alreadyClaimedTrieRoot = emptyTrieRoot
      --               redemptionsTrieRoot = emptyTrieRoot
      --               redemptionsTreeRoot = MerkleHash ""
      --               redemptionsCount = 0
      --               redemptionsSum = 0
      --             }` in BuiltinData representation.
      --  3.3. All other outputs must contain only ADA.
      -- 4. Signature Validation:
      --  4.1. The transaction must contain at-least `dynamicParams.minAuthSignatures` signatures from `dynamicParams.tgeAgentAuthKeys`.
      NightMintStar ->
        case txNightInfoOutputs of
          TxOut nightOutAddress nightOutValue nightOutDatum _
            : TxOut hydraOutAddress hydraOutValue hydraOutDatum _
            : change ->
              withMaybeExtraToken nightOutValue $ \_nightAda nightExtra ->
                -- 3.1.3 (validates that it contains only ada and one non-ada token)
                -- to do. ensure `hydraAda` contains required collateral?
                withMaybeExtraToken hydraOutValue $ \_hydraAda hydraExtra ->
                  -- 3.2.3 (validates that it contains only ada and one non-ada token)
                  checkNightMintStar
                    nightOutAddress
                    nightOutDatum
                    nightExtra
                    hydraOutAddress
                    hydraOutDatum
                    hydraExtra
                    change
          _ -> trace "number of outputs" False
      RemintHydraThread ->
        case txNightInfoOutputs of
          TxOut hydraOutAddress hydraOutValue hydraOutDatum _ : change ->
            withMaybeExtraToken hydraOutValue $ \_hydraAda hydraExtra ->
              ( case M.toList mintedValue of
                  [(hydraT, hydraCount)] ->
                    True
                      && isHydraThreadToken hydraT
                      && traceIfFalse "minted hydra thread token" (hydraCount == 1)
                      && case hydraExtra of
                        Nothing -> trace "missing hydra extra" False
                        Just (hydraExtraPolicyId, hydraExtraToken, hydraExtraAmount) ->
                          True
                            && traceIfFalse "hydra extra policy id" (hydraExtraPolicyId == policyId)
                            && traceIfFalse "hydra extra token" (hydraExtraToken == hydraT)
                            && traceIfFalse "hydra extra amount" (hydraExtraAmount == 1)
                  _ -> trace "minted value" False
              )
                -- inputs
                && traceIfFalse
                  "ada inputs"
                  (all (isChangeOutput . txInInfoResolved) txNightInfoInputs)
                && traceIfFalse
                  "public key inputs"
                  ( all
                      ( \case
                          TxInInfo _ (TxOut (Address (PubKeyCredential _) _) _ _ _) -> True
                          _ -> False
                      )
                      txNightInfoInputs
                  )
                -- hydra thread output
                && traceIfFalse
                  "hydra out address credential"
                  (addressCredential hydraOutAddress == ScriptCredential hydraThreadScriptHash)
                && traceIfFalse
                  "hydra out datum"
                  ( hydraOutDatum
                      == serializeDatum
                        ( HydraThreadClaiming
                            $ HT.MakeHydraThreadClaiming
                              { HT.allotmentsTreeRoot = merkleRoot
                              , HT.allotmentsTreeDepth = treeDepth
                              , HT.alreadyClaimedTrieRoot = emptyTrieRoot
                              , HT.redemptionsSum = 0
                              }
                        )
                  ) -- 3.2.5
                  -- signatures
                && traceIfFalse
                  "signatures"
                  ( minAuthSignatures
                      <= intersectionLength
                        (unsafeMakeAscending tgeAgentAuthKeys)
                        (makeAscending txNightInfoSignatories)
                  )
                && traceIfFalse "change" (all isChangeOutput change)
          _ -> trace "number of outputs" False
      NightBurnGenesis nightGenesis ->
        -- script inputs only
        mapMaybe
          ( \case
              TxInInfo _ (TxOut (Address (ScriptCredential h) _) v d _) -> Just (h, v, d)
              _ -> Nothing
          )
          txNightInfoInputs
          & \case
            [ (nightS, nightValue, nightDatumSerialized)
              , (hydraS, hydraValue, hydraDatumSerialized)
              ]
                | nightS == coerce policyId
                , hydraS == hydraThreadScriptHash ->
                    Just ((nightValue, nightDatumSerialized), (hydraValue, hydraDatumSerialized))
            [ (hydraS, hydraValue, hydraDatumSerialized)
              , (nightS, nightValue, nightDatumSerialized)
              ]
                | nightS == coerce policyId
                , hydraS == hydraThreadScriptHash ->
                    Just ((nightValue, nightDatumSerialized), (hydraValue, hydraDatumSerialized))
            _ -> Nothing
          & \case
            Nothing -> trace "night or hydra input" False
            Just ((nightValue, _nightDatumSerialized), (hydraValue, hydraDatumSerialized)) ->
              withMaybeExtraToken nightValue $ \_nightAda nightExtra ->
                withMaybeExtraToken hydraValue $ \_hydraAda hydraExtra ->
                  deserializeDatum hydraDatumSerialized $ \case
                    HT.HydraThreadFinal hydraDatum ->
                      case txNightInfoOutputs of
                        TxOut supplyAddress supplyValue supplyDatum _
                          : outputs ->
                            withMaybeExtraToken supplyValue $ \_supplyAda supplyExtra ->
                              checkNightBurnGenesis
                                nightGenesis
                                nightExtra
                                hydraExtra
                                hydraDatum
                                supplyAddress
                                supplyExtra
                                supplyDatum
                                outputs
                        _ -> trace "number of outputs 0" False
                    _ -> trace "wrong hydra state" False
      DynamicMintingLogic ->
        -- This optimization is necessary as this redeemer path is expected to execute frequently
        -- in the future for SPO reward payouts for partnerchains framework.
        -- Plinth dead code optimization should ensure that this branch avoids unnecessary decoding of builtin-data.
        fromOpaque
          $ checkWithdrawZero dynamicMintingLogic
          $ BI.unsafeDataAsMap txNightInfoWdrl
    where
      checkNightMintStar nightOutAddress nightOutDatum nightExtra hydraOutAddress hydraOutDatum hydraExtra change =
        True
          -- mint assets
          && ( M.toList mintedValue
                & \case
                  [(starT, starCount), (hydraT, hydraCount)]
                    | starT == starToken
                    , isHydraThreadToken hydraT ->
                        Just ((starT, starCount), (hydraT, hydraCount))
                  [(hydraT, hydraCount), (starT, starCount)]
                    | starT == starToken
                    , isHydraThreadToken hydraT ->
                        Just ((starT, starCount), (hydraT, hydraCount))
                  _ -> Nothing
                & \case
                  Nothing -> trace "minted value" False
                  Just ((_starT, starCount), (hydraT, hydraCount)) ->
                    True
                      && traceIfFalse "minted star" (starCount == supply)
                      && traceIfFalse "minted hydra thread token" (hydraCount == 1)
                      && case hydraExtra of
                        Nothing -> trace "missing hydra extra" False
                        Just (hydraExtraPolicyId, hydraExtraToken, hydraExtraAmount) ->
                          True
                            && traceIfFalse "hydra extra policy id" (hydraExtraPolicyId == policyId)
                            && traceIfFalse "hydra extra token" (hydraExtraToken == hydraT) -- 3.2.3 (validates that the non-ada token is the hydra thread token)
                            && traceIfFalse "hydra extra amount" (hydraExtraAmount == 1)
             )
          -- inputs
          && case txNightInfoInputs of
            [TxInInfo txOutRef (TxOut _ seedValue _ _)] -> traceIfFalse "seed" (txOutRef == seedInput && isAdaOnly seedValue) -- 1 && 1.1 && 1.2
            _ -> trace "number of inputs" False
          -- night output
          && traceIfFalse
            "night out address credential"
            (addressCredential nightOutAddress == ScriptCredential (coerce policyId)) -- 3.1.1
          && case nightExtra of
            Nothing -> trace "missing night extra" False
            Just (nightExtraPolicyId, nightExtraToken, nightExtraAmount) ->
              True
                && traceIfFalse "night extra policy id" (nightExtraPolicyId == policyId)
                && traceIfFalse "night extra token" (nightExtraToken == starToken) -- 3.1.3 (validates that the non-ada token is the star token)
                && traceIfFalse "night extra amount" (nightExtraAmount == supply)
          && traceIfFalse "night out datum" (nightOutDatum == serializeDatum ()) -- 3.1.5
          -- hydra thread output
          && traceIfFalse
            "hydra out address credential"
            (addressCredential hydraOutAddress == ScriptCredential hydraThreadScriptHash)
          && traceIfFalse
            "hydra out datum"
            ( hydraOutDatum
                == serializeDatum
                  ( HydraThreadClaiming
                      $ HT.MakeHydraThreadClaiming
                        { HT.allotmentsTreeRoot = merkleRoot
                        , HT.allotmentsTreeDepth = treeDepth
                        , HT.alreadyClaimedTrieRoot = emptyTrieRoot
                        , HT.redemptionsSum = 0
                        }
                  )
            ) -- 3.2.5
            -- signatures
          && traceIfFalse
            "signatures"
            ( minAuthSignatures
                <= intersectionLength
                  (unsafeMakeAscending tgeAgentAuthKeys)
                  (makeAscending txNightInfoSignatories)
            ) -- 4.1
          && traceIfFalse "change" (all isChangeOutput change) -- 3.3
      checkNightBurnGenesis
        nightGenesis
        nightExtra
        hydraExtra
        hydraDatum
        supplyAddress
        supplyExtra
        supplyDatum
        outputs =
          let claimTarget = divide supply 5 -- want 20% to claim + scavenge + lost&found
              minPostClaim = divide supply 100 -- 1% minimum (each) to scavenge and lost&found
              postClaim = max minPostClaim (divide (claimTarget - getStar redemptionsSum) 2)
              -- We are OK with rounding towards zero. In fact, that's why we're using div.
              -- We always have to round towards zero with STAR computations, because we cannot allow
              -- fractions, and we cannot allow the rounded sums to add up to more than the amount of STAR that exists.
              -- This means that the compute amounts tend slightly to the smaller side, and any extra
              -- leftover amounts will simply go to the Supply contract.
              effectiveRedemptionSum = getStar redemptionsSum + postClaim
           in True
                -- night input
                && case nightExtra of
                  Nothing -> trace "missing night extra" False
                  Just (nightExtraPolicyId, nightExtraToken, nightExtraAmount) ->
                    True
                      && traceIfFalse "night extra policy id" (nightExtraPolicyId == policyId)
                      && traceIfFalse "night extra token" (nightExtraToken == starToken)
                      && traceIfFalse "night extra amount" (nightExtraAmount == supply)
                -- 3.1.4
                -- hydra thread input
                && case hydraExtra of
                  Nothing -> trace "missing hydra extra" False
                  Just (hydraExtraPolicyId, hydraExtraToken, hydraExtraAmount) ->
                    True
                      && traceIfFalse "hydra extra policy id" (hydraExtraPolicyId == policyId)
                      && traceIfFalse "hydra extra token" (isHydraThreadToken hydraExtraToken)
                      && traceIfFalse "hydra extra amount" (hydraExtraAmount == 1)
                -- redemption supply output
                && traceIfFalse
                  "supply address"
                  ( supplyAddress == supplyScriptAddress
                  )
                && case supplyExtra of
                  Nothing -> trace "missing supply extra" False
                  Just (supplyExtraPolicyId, supplyExtraToken, supplyExtraAmount) ->
                    True
                      && traceIfFalse "supply extra policy id" (supplyExtraPolicyId == policyId)
                      && traceIfFalse "supply extra token" (supplyExtraToken == starToken)
                      && traceIfFalse
                        "supply extra amount"
                        (Star supplyExtraAmount == Star effectiveRedemptionSum)
                && deserializeDatum supplyDatum \case
                  RedemptionSupplyDatum
                    _rootHash
                    (GenesisTimestamp genesisTimestampNew)
                    redemptionIncrementPeriodNew
                    (Subdividing _redemptionsTreeDepth) ->
                      True
                        && traceIfFalse "supply genesis" (genesisTimestampNew == genesisTimestamp)
                        && traceIfFalse
                          "supply increment period"
                          (redemptionIncrementPeriodNew == redemptionIncrementPeriod)
                  _ -> trace "supply state" False
                -- check that genesisTimestamp is in the range [now; now + 6 months)
                && traceIfFalse
                  "genesis timestamp"
                  ( let (validityRangeStart, validityRangeEnd) = finiteTxValidityRange txNightInfoValidRange
                        DiffMilliSeconds left = genesisTimestamp `diffTime` coerce validityRangeEnd
                        milliSecondsPerDay = 86400 * 1000
                        sixMonthsFromNow = POSIXTime $ getPOSIXTime (coerce validityRangeEnd) + milliSecondsPerDay * 183
                        DiffMilliSeconds right = sixMonthsFromNow `diffTime` genesisTimestamp
                     in left
                          >= 0
                          && right
                          > 0
                          && (validityRangeEnd - validityRangeStart <= milliSecondsPerDay)
                  )
                -- treasury, foundation wallets, and change outputs
                && checkTreasuryFoundationChange
                  treasuryScriptHash
                  (Star effectiveRedemptionSum)
                  outputs
                -- mint assets
                && traceIfFalse "night mint" (checkHydraNotMinted $ M.toList mintedValue)
                -- signatures
                && traceIfFalse
                  "signatures"
                  ( minAuthSignatures
                      <= intersectionLength
                        (unsafeMakeAscending tgeAgentAuthKeys)
                        (makeAscending txNightInfoSignatories)
                  )
          where
            NightGenesis treasuryScriptHash genesisTimestamp redemptionIncrementPeriod = nightGenesis
            HT.MakeHydraThreadFinal{redemptionsSum} = hydraDatum
      checkHydraNotMinted [(token, c)] = isHydraThreadToken token && c == (-1)
      checkHydraNotMinted _ = False
      -- Diagram of disbursement logic:
      --                  |        /       /
      --                  |       /       /
      -- foundation share |      /       /
      --                  |     /       /
      --                  |____/       /   -
      --                  |           /     |
      --                  |          /      |
      --                  |         /       |
      --                  |        /        | }- min treasury share
      --   treasury share |       /         |
      --                  |      /          |
      --                  |     /           |
      --                  |    /           -
      --                  |   /
      --                  |  /
      --                  | /
      --                  |/_______________
      --
      --  If possible, the full amount is paid out to the foundation
      --  wallets and the rest goes into the treasury. If the amount that
      --  would go to the treasury is below the minimum, the foundation
      --  wallet shares are scaled down proportionally.
      checkTreasuryFoundationChange treasuryScriptHash (Star effectiveRedemptionSum) outputs =
        let starRemaining = supply - effectiveRedemptionSum
            foundationTotalAdjusted = starRemaining - minTreasuryTokens
            foundationTotalUnadjusted = sum (snd <$> foundationWallets)
            foundationWalletsAdjusted
              | foundationTotalAdjusted <= 0 = mempty
              | foundationTotalAdjusted < foundationTotalUnadjusted =
                  (map . second)
                    ((`divide` foundationTotalUnadjusted) . (* foundationTotalAdjusted))
                    foundationWallets
              | otherwise = foundationWallets
            treasuryTokens = starRemaining - sum (snd <$> foundationWalletsAdjusted)
         in if treasuryTokens == 0
              then all isChangeOutput outputs
              else case outputs of
                TxOut treasuryOutAddress treasuryOutValue _ _
                  : foundationAndChangeOutputs ->
                    withMaybeExtraToken treasuryOutValue $ \_treasuryAda treasuryExtra ->
                      True
                        -- treasury output
                        && traceIfFalse
                          "treasury address"
                          (addressCredential treasuryOutAddress == ScriptCredential treasuryScriptHash)
                        && case treasuryExtra of
                          Nothing -> trace "missing treasury extra" False
                          Just (nightExtraPolicyId, nightExtraToken, nightExtraAmount) ->
                            True
                              && traceIfFalse "night extra policy id" (nightExtraPolicyId == policyId)
                              && traceIfFalse "night extra token" (nightExtraToken == starToken)
                              && traceIfFalse "night extra amount" (nightExtraAmount == treasuryTokens)
                        -- foundation wallets outputs
                        && splitAtExactly (length foundationWalletsAdjusted) foundationAndChangeOutputs \(foundationOutputs, changeOutputs) ->
                          all
                            ( \(TxOut foundAddress foundValue _ _, (expectedAddress, expectedStar)) ->
                                withMaybeExtraToken foundValue $ \_foundAda foundExtra ->
                                  True
                                    && traceIfFalse "foundation address" (foundAddress == expectedAddress)
                                    && case foundExtra of
                                      Nothing -> trace "missing night extra" False
                                      Just (nightExtraPolicyId, nightExtraToken, nightExtraAmount) ->
                                        True
                                          && traceIfFalse "night extra policy id" (nightExtraPolicyId == policyId)
                                          && traceIfFalse "night extra token" (nightExtraToken == starToken)
                                          && traceIfFalse "night extra amount" (nightExtraAmount == expectedStar)
                            )
                            (zip foundationOutputs foundationWalletsAdjusted)
                            -- change outputs
                            && traceIfFalse "change 1" (all isChangeOutput changeOutputs)
                _ -> trace "number of outputs 1" False
      NightParams
        { merkleRoot
        , treeDepth
        , seedInput
        , minTreasuryTokens
        , supply
        , protocolParamsCS
        } = params

      NightMintingParams
        { dynamicMintingLogic
        , hydraThreadScriptHash
        , supplyScriptAddress
        , foundationWallets
        , tgeAgentAuthKeys
        , minAuthSignatures
        } =
          getDynamicParams protocolParamsCS
            $ BI.unsafeDataAsList txNightInfoReferenceInputs

{-# INLINEABLE nightSpend #-}
nightSpend
  :: NightParams
  -> NightTxInfo
  -> TxOutRef
  -> ScriptHash
  -> Maybe StakingCredential
  -> Value
  -> ()
  -> ()
  -> Bool
nightSpend _ (NightTxInfo{txNightInfoRedeemers}) _ (coerce -> policyId) _ _ _ _ =
  case M.lookup (Minting policyId) txNightInfoRedeemers of
    Just (Redeemer redeemer) | Just (NightBurnGenesis{}) <- fromBuiltinData redeemer -> True
    Just (Redeemer redeemer) | Just (DynamicMintingLogic{}) <- fromBuiltinData redeemer -> True
    _ -> trace "night NightBurnGenesis redeemer" False

-- mostly copied from https://plutus.cardano.intersectmbo.org/haddock/1.38.0.0/plutus-tx/src/PlutusTx.List.html#splitAt
{-# INLINEABLE splitAtExactly #-}
splitAtExactly :: Integer -> [a] -> ContBool ([a], [a])
splitAtExactly n xs continue0
  | n <= 0 = continue0 ([], xs)
  | otherwise = go n xs continue0
  where
    go :: Integer -> [a] -> ContBool ([a], [a])
    go m (y : ys) continue1
      | m == 1 = continue1 ([y], ys)
      | otherwise = go (m - 1) ys $ \(zs, ws) -> continue1 (y : zs, ws)
    go _ _ _ =
      trace ("`splitAtExactly` index argument " <> showDebug n <> " to small") False

stripAdaValue :: Value -> Value
stripAdaValue v = Value $ M.unsafeFromList $ tail $ M.toList $ getValue v
