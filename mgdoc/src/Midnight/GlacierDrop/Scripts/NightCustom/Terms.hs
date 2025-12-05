{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}
module Midnight.GlacierDrop.Scripts.NightCustom.Terms where

import Data.Bifunctor (Bifunctor (second))
import Midnight.GlacierDrop.Api (MerkleHash (..), Star (..))
import Midnight.GlacierDrop.NightApi (
  BuiltinTxInputs,
 )
import Midnight.GlacierDrop.Scripts.Common (
  ContBool,
  deserializeDatum,
  intersectionLength,
  isChangeOutput,
  isToBurnToken,
  newNightTokenName,
  showDebug,
  trace,
  traceIfFalse,
  withMaybeExtraToken,
  withMaybeExtraToken2,
 )
import Midnight.GlacierDrop.Scripts.NightCustom.Types (
  NightCustomParams (
    NightCustomParams,
    genesisTimestamp,
    policyId,
    protocolParamsCS
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
  RedemptionIncrementPeriod (RedemptionIncrementPeriod),
 )
import PlutusLedgerApi.V1.Time (DiffMilliSeconds (..), POSIXTime (..))
import PlutusLedgerApi.V3 (
  Address (Address, addressCredential),
  Credential (ScriptCredential),
  CurrencySymbol (CurrencySymbol),
  ScriptContext (..),
  ScriptHash (ScriptHash),
  TxInInfo (TxInInfo),
  TxInfo (..),
  TxOut (TxOut),
  mintValueToMap,
 )
import PlutusPrelude (coerce, (&))
import PlutusTx qualified
import PlutusTx.AssocMap qualified as M
import PlutusTx.Builtins qualified as EBI
import PlutusTx.Builtins.HasOpaque qualified as BI
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Foldable (sum)
import PlutusTx.List (all, length, map, zip)
import PlutusTx.Prelude hiding (error, trace, traceError, traceIfFalse)
import Refined (makeAscending, unsafeMakeAscending)

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

{-# INLINEABLE nightStake #-}
nightStake :: NightCustomParams -> ScriptContext -> Bool
nightStake
  params
  ScriptContext
    { scriptContextTxInfo =
      TxInfo
        { txInfoReferenceInputs
        , txInfoInputs
        , txInfoOutputs
        , txInfoSignatories
        , txInfoMint
        }
    } =
    -- script inputs only
    mapMaybe
      ( \case
          TxInInfo _ (TxOut (Address (ScriptCredential h) _) v d _) -> Just (h, v, d)
          _ -> Nothing
      )
      txInfoInputs
      & \case
        [(nightS, nightValue, nightDatumSerialized)]
          | nightS == coerce policyId ->
              Just (nightValue, nightDatumSerialized)
        _ -> Nothing
      & \case
        Nothing -> trace "night input" False
        Just (nightValue, _nightDatumSerialized) ->
          withMaybeExtraToken2 nightValue $ \_nightAda nightExtra1 nightExtra2 ->
            let nightExtra = pickNightToken nightExtra1 nightExtra2
             in case txInfoOutputs of
                  TxOut supplyAddress supplyValue supplyDatum _
                    : outputs ->
                      withMaybeExtraToken supplyValue $ \_supplyAda supplyExtra ->
                        checkNightBurnGenesis
                          nightExtra
                          supplyAddress
                          supplyExtra
                          supplyDatum
                          outputs
                  _ -> trace "number of outputs 0" False
    where
      checkNightBurnGenesis
        nightExtra
        supplyAddress
        supplyExtra
        supplyDatum
        outputs =
          case nightExtra of
            Nothing -> trace "missing night extra" False
            Just (nightExtraPolicyId, nightExtraToken, nightExtraAmount) ->
              True
                && traceIfFalse "night extra policy id" (nightExtraPolicyId == policyId)
                && traceIfFalse "night extra token" (nightExtraToken == newNightTokenName)
                && traceIfFalse "night extra amount" (nightExtraAmount == hardcodedSupply)
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
                  && traceIfFalse "supply extra token" (supplyExtraToken == newNightTokenName)
                  && traceIfFalse
                    "supply extra amount"
                    (Star supplyExtraAmount == Star hardcodedEffectiveRedemptionSum)
            && deserializeDatum supplyDatum \case
              RedemptionSupplyDatum
                _rootHash
                (GenesisTimestamp genesisTimestampNew)
                redemptionIncrementPeriodNew
                (Subdividing _redemptionsTreeDepth) ->
                  True
                    -- && traceIfFalse "supply root hash" (_rootHash == hardcodedRedemptionsTreeRoot)
                    && traceIfFalse "supply genesis" (genesisTimestampNew == genesisTimestamp)
                    && traceIfFalse
                      "supply increment period"
                      (redemptionIncrementPeriodNew == hardcodedRedemptionIncrementPeriod)
              _ -> trace "supply state" False
            -- treasury, foundation wallets, and change outputs
            && checkTreasuryFoundationChange outputs
            -- mint assets
            && ( case M.lookup policyId (mintValueToMap txInfoMint) of
                  Just mintedValueForPolicy ->
                    traceIfFalse "night mint" (checkHydraNotMinted $ M.toList mintedValueForPolicy)
                  Nothing -> trace "minted policy id" False
               )
            -- signatures
            && traceIfFalse
              "signatures"
              ( minAuthSignatures
                  <= intersectionLength
                    (unsafeMakeAscending tgeAgentAuthKeys)
                    (makeAscending txInfoSignatories)
              )
      checkHydraNotMinted [(token, c)] = isToBurnToken token && c == (-1)
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
      checkTreasuryFoundationChange outputs =
        let starRemaining = hardcodedSupply - hardcodedEffectiveRedemptionSum
            foundationTotalAdjusted = starRemaining - hardcodedMinTreasuryTokens
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
                          (addressCredential treasuryOutAddress == addressCredential reserveScriptAddress)
                        && case treasuryExtra of
                          Nothing -> trace "missing treasury extra" False
                          Just (nightExtraPolicyId, nightExtraToken, nightExtraAmount) ->
                            True
                              && traceIfFalse "night extra policy id" (nightExtraPolicyId == policyId)
                              && traceIfFalse "night extra token" (nightExtraToken == newNightTokenName)
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
                                          && traceIfFalse "night extra token" (nightExtraToken == newNightTokenName)
                                          && traceIfFalse "night extra amount" (nightExtraAmount == expectedStar)
                            )
                            (zip foundationOutputs foundationWalletsAdjusted)
                            -- change outputs
                            && traceIfFalse "change 1" (all isChangeOutput changeOutputs)
                _ -> trace "number of outputs 1" False
      NightCustomParams
        { protocolParamsCS
        , policyId
        , genesisTimestamp
        } = params

      pickNightToken nightExtra1 nightExtra2 =
        case (nightExtra1, nightExtra2) of
          (Just extra@(_, token, _), _) | token == newNightTokenName -> Just extra
          (_, Just extra@(_, token, _)) | token == newNightTokenName -> Just extra
          (Just extra, Nothing) -> Just extra
          (Nothing, Just extra) -> Just extra
          _ -> Nothing

      NightMintingParams
        { supplyScriptAddress
        , foundationWallets
        , tgeAgentAuthKeys
        , minAuthSignatures
        , reserveScriptAddress
        } =
          getDynamicParams protocolParamsCS
            $ BI.unsafeDataAsList
            $ toBuiltinData txInfoReferenceInputs

{-# INLINEABLE hardcodedMinTreasuryTokens #-}
hardcodedMinTreasuryTokens :: Integer
hardcodedMinTreasuryTokens = 0

{-# INLINEABLE hardcodedSupply #-}
hardcodedSupply :: Integer
hardcodedSupply = 24_000_000_000_000_000

-- | This is the effective redemption sum for the redemption supply.
-- It is the combination of the night that was claimed in the glacier drop and the night that was claimed in the scavenge phase.
-- The night that was claimed in the scavenge phase is 999,999,999,126,012 (this is as close as we could get to distributing 1 billion night in the scavenger phase)
-- The night that was claimed in the glacier drop is 3,547,399,401,508,942 (this value matches the redemption sum present in the HydraThreadOutput datum).
-- So the effective redemption sum is 4,547,399,400,634,954.
{-# INLINEABLE hardcodedEffectiveRedemptionSum #-}
hardcodedEffectiveRedemptionSum :: Integer
hardcodedEffectiveRedemptionSum = 4_547_399_400_634_954

-- | This is the redemption increment period for the redemption supply.
-- It is set to 90 days.
{-# INLINEABLE hardcodedRedemptionIncrementPeriod #-}
hardcodedRedemptionIncrementPeriod :: RedemptionIncrementPeriod
hardcodedRedemptionIncrementPeriod = RedemptionIncrementPeriod (DiffMilliSeconds 7_776_000_000)

-- This is the genesis timestamp of 2025-12-10 00:00 UTC in Plutus POSIXTime
-- Plutus POSIXTime is measured as the number of /milliseconds/ since 1970-01-01T00:00:00Z.
-- This is not the same as Haskell's `Data.Time.Clock.POSIX.POSIXTime`
{-# INLINEABLE _hardcodedGenesisTimestamp #-}
_hardcodedGenesisTimestamp :: POSIXTime
_hardcodedGenesisTimestamp = 1_765_324_800_000

{-# INLINEABLE hardcodedRedemptionsTreeRoot #-}
hardcodedRedemptionsTreeRoot :: MerkleHash
hardcodedRedemptionsTreeRoot =
  MerkleHash
    $ BI.stringToBuiltinByteStringHex
      "bda0ad8497f79402c657b0148d7c0d832bb831909b702ccdb0497a1d4d3df500"
