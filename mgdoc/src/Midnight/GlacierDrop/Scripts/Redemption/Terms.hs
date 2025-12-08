module Midnight.GlacierDrop.Scripts.Redemption.Terms where

import Data.Void (Void)
import Midnight.GlacierDrop.Api (
  ScriptContext,
  TxInfo (
    TxInfo,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoValidRange
  ),
 )
import Midnight.GlacierDrop.Scripts.Common (
  mkSpendingScript,
  newNightTokenName,
  serializeDatum,
  trace,
  traceIfFalse,
  withAnExtraToken,
 )
import Midnight.GlacierDrop.Scripts.Redemption.Types (
  RedemptionDatum (..),
  RedemptionRedeemer (..),
 )
import Midnight.GlacierDrop.Scripts.ThawingSchedule (
  IncrementsLeft (IncrementsLeft),
  StarAmount (StarAmount),
  ThawTime (ThawTime),
  ThawingScheduleReduction (
    ThawingScheduleFullyReduced,
    ThawingScheduleNotReduced,
    ThawingSchedulePartiallyReduced
  ),
  pastPointFromValidityInterval,
  reduceThawingSchedule,
 )
import PlutusLedgerApi.V1.Value (Value (..), isZero)
import PlutusLedgerApi.V3 (
  Address (..),
  Credential (..),
  ScriptHash,
  StakingCredential,
  TxInInfo (..),
  TxOut (..),
  TxOutRef,
 )
import PlutusTx.List (all)
import PlutusTx.Prelude hiding (trace, traceError, traceIfFalse)

{-# INLINEABLE redemptionScript #-}
redemptionScript
  :: ()
  -> ScriptContext Void RedemptionRedeemer RedemptionDatum
  -> Bool
redemptionScript = mkSpendingScript . spendRedemption

-- Inputs Validation:
--  1. The transaction must contain exactly one script input, `ownInput`, the input that is being validated by the invocation of this spending script.
--    1.1 The input must contain only Ada and one non-ada token with the token name `newNightTokenName`.
-- Mint Validation:
--  2. The minted value must be empty.
-- Output Validation:
--  3. There are two cases:
--    3.1 The thawing schedule is fully reduced, in which case:
--     3.1.1 There must be an output, `thawOutput`, with the following properties:
--      3.1.1.1 The address of the output must be the destination address specified in the `ownInput`'s datum.
--      3.1.1.2 The value of the output must contain the total amount of the non-ada token in `ownInput`.
--      3.1.1.3 The non-ada token must have the token name `newNightTokenName`.
--    3.2 The thawing schedule is partially reduced, in which case:
--     3.2.1 There must be at-least two outputs, `thawOutput` and `redemptionOutput`, with the following properties:
--      Thaw Output:
--        3.2.1.1 The address of the `thawOutput` must be the destination address specified in the `ownInput`'s datum.
--        3.2.1.2 The value of the `thawOutput` must contain `thawedAmount` of non-ada token where
--          `thawedAmount = incrementAmountQuotient * thawedIncrements`
--          and
--          `thawedIncrements` is the number of `incrementPeriod`s that have passed since the last thaw.
--        3.2.1.3 The non-ada token must have the token name `newNightTokenName`.
--        3.2.1.4 The value of the `thawOutput` must contain only ada and a single non-ada token that is the same as the non-ada token in `ownInput`.
--      Redemption Output:
--        3.2.2.1 The address of the `redemptionOutput` must be equal to the address of `ownInput`.
--        ^ The above is how it should be, but the current implementation only enforces that the payment credential is equal to the input payment credential.
--        3.2.2.2 The quantity of the non-ada token in `redemptionOutput` must be the quantity of the non-ada token in `ownInput` minus the quantity of the non-ada token in `thawOutput`.
--        3.2.2.3 The non-ada token must have the token name `newNightTokenName`.
--        3.2.2.4 The quantity of ada in `redemptionOutput` must be equal to or greater than the quantity of ada in `ownInput`.
--        3.2.2.5 The value of the `redemptionOutput` must contain only ada and a single non-ada token that is the same as the non-ada token in `thawOutput`.
--        3.2.2.6 The datum of the `redemptionOutput` must respect the following invariants:
--          destinationAddress ownOutputDatum == destinationAddress ownInputDatum
--          incrementAmountQuotient ownOutputDatum == incrementAmountQuotient ownInputDatum
--          redemptionIncrementPeriod ownOutputDatum == redemptionIncrementPeriod ownInputDatum
{-# INLINEABLE spendRedemption #-}
spendRedemption
  :: ()
  -> TxInfo
  -> TxOutRef
  -> ScriptHash
  -> Maybe StakingCredential
  -> Value
  -> RedemptionRedeemer
  -> RedemptionDatum
  -> Bool
spendRedemption
  _
  (TxInfo{txInfoInputs, txInfoValidRange, txInfoOutputs, txInfoMint})
  _
  ownHash
  stakingCredential
  inValue
  Thaw
  ( RedemptionDatum
      destinationAddress
      incrementAmountQuotient
      nextThaw
      incrementsLeft
      redemptionIncrementPeriod
    ) =
    withAnExtraToken inValue $ \inAda inExtraPolicyId inExtraToken inExtraAmount ->
      -- 1.1
      pastPointFromValidityInterval txInfoValidRange $ \now ->
        case txInfoOutputs of
          thawOutput@(TxOut _ thawValue _ _) : outputs ->
            withAnExtraToken thawValue $ \_ thawExtraPolicyId thawExtraToken thawExtraAmount ->
              -- 3.2.1.4
              case reduceThawingSchedule
                redemptionIncrementPeriod
                now
                (StarAmount incrementAmountQuotient)
                (ThawTime nextThaw)
                (IncrementsLeft incrementsLeft) of
                ThawingScheduleNotReduced -> trace "no thaw" False
                ThawingScheduleFullyReduced ->
                  checkThaw
                    inAda
                    inExtraPolicyId
                    inExtraToken
                    inExtraAmount
                    thawOutput
                    thawExtraPolicyId
                    thawExtraToken
                    thawExtraAmount
                    outputs
                    Nothing
                ThawingSchedulePartiallyReduced thawedAmount nextThawNew incrementsLeftNew ->
                  checkThaw
                    inAda
                    inExtraPolicyId
                    inExtraToken
                    inExtraAmount
                    thawOutput
                    thawExtraPolicyId
                    thawExtraToken
                    thawExtraAmount
                    outputs
                    (Just (thawedAmount, nextThawNew, incrementsLeftNew))
          _ -> trace "number of outputs" False
    where
      checkThaw
        inAda
        inExtraPolicyId
        inExtraToken
        inExtraAmount
        thawOutput
        thawExtraPolicyId
        thawExtraToken
        thawExtraAmount
        outputs
        thawSchedule =
          True
            -- redemption input
            && traceIfFalse "in extra token" (inExtraToken == newNightTokenName) -- 1.1
            -- other inputs
            -- TODO[auditors]. we are deliberately allowing non ada tokens like night tokens in other inputs. is this safe?
            && traceIfFalse
              "public key inputs"
              ( all
                  ( \case
                      TxInInfo _ (TxOut (Address (PubKeyCredential _) _) _ _ _) -> True
                      _ -> False
                  )
                  txInfoInputs
              ) -- 1 (enforces that there is exactly one script input, the input that is being validated by the invocation of this spending script)
              -- thaw output
            && traceIfFalse "thaw out address" (thawAddress == destinationAddress) -- 3.2.1.1
            && traceIfFalse "thaw out extra policy id" (thawExtraPolicyId == inExtraPolicyId) -- 3.2.1.4
            && traceIfFalse "thaw out extra token" (thawExtraToken == newNightTokenName) -- 3.2.1.3
            && traceIfFalse
              "thaw out extra amount"
              ( thawExtraAmount
                  == maybe
                    inExtraAmount
                    (\(StarAmount thawedAmount, _, _) -> thawedAmount)
                    thawSchedule
              ) -- 3.1.1.2 || 3.2.1.2
              -- mint
            && traceIfFalse "mint" (isZero txInfoMint) -- 2
            -- redemption output
            && case thawSchedule of
              Nothing ->
                -- TODO[auditors]. we are deliberately not imposing any constraints on the remaining `outputs`. is this safe?
                True
              Just (thawedAmount, ThawTime nextThawNew, IncrementsLeft incrementsLeftNew) ->
                case outputs of
                  TxOut outAddress outValue outDatum _ : _remainingOutputs ->
                    withAnExtraToken outValue $ \outAda outExtraPolicyId outExtraToken outExtraAmount ->
                      -- 3.2.2.5 (enforces that the `redemptionOuput` value contains only ada and a single non-ada token)
                      True
                        -- TODO[auditors]. we are deliberately not imposing any constraints on the `_remainingOutputs`. is this safe?
                        && traceIfFalse "out ada" (inAda <= outAda) -- 3.2.2.4
                        && traceIfFalse
                          "redemption out address"
                          (outAddress == Address (ScriptCredential ownHash) stakingCredential) -- 3.2.2.1
                        && traceIfFalse
                          "redemption out extra policy id"
                          (outExtraPolicyId == inExtraPolicyId) -- 3.2.2.5 (enforces that the non-ada token in the `redemptionOutput` is the same as the non-ada token in `ownInput`)
                        && traceIfFalse "redemption out extra token" (outExtraToken == newNightTokenName) -- 3.2.2.3
                        && traceIfFalse
                          "redemption out extra amount"
                          (thawedAmount == StarAmount (inExtraAmount - outExtraAmount)) -- 3.2.2.2
                        && traceIfFalse
                          "redemption out datum"
                          ( outDatum
                              == serializeDatum
                                ( RedemptionDatum
                                    destinationAddress
                                    incrementAmountQuotient
                                    nextThawNew
                                    incrementsLeftNew
                                    redemptionIncrementPeriod
                                ) -- 3.2.2.6
                          )
                  _ -> trace "number of outputs" False -- 3.2.1
          where
            TxOut thawAddress _ _ _ = thawOutput
