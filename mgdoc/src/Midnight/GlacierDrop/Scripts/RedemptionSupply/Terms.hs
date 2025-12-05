module Midnight.GlacierDrop.Scripts.RedemptionSupply.Terms (
  checkProof,
  mkLeafHash,
  markClaim,
  merkleHash,
  redemptionSupplyScript,
) where

import Data.Function ((&))
import Data.Void (Void)
import GHC.ByteOrder (ByteOrder (BigEndian))
import Midnight.GlacierDrop.Api (
  MerkleHash (MerkleHash),
  ScriptContext (ScriptContext),
  ScriptInfo (SpendingScript),
  TxInfo (
    TxInfo,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoValidRange
  ),
 )
import Midnight.GlacierDrop.Scripts.Common (
  SelfSeparatedTxInfo (SelfSeparatedTxInfo),
  hashNode,
  isAdaOnly,
  newNightTokenName,
  serializeDatum,
  trace,
  traceError,
  traceIfFalse,
  withAnExtraToken,
  withContinuingOutputs,
  withMaybeExtraToken,
  withSelfSeparatedTxInfo,
 )
import Midnight.GlacierDrop.Scripts.Redemption.Types (
  RedemptionDatum (RedemptionDatum),
 )
import Midnight.GlacierDrop.Scripts.RedemptionSupply.Types (
  MaterializationInfo (..),
  MaterializationMask (..),
  MerkleTreeDepth (..),
  RedemptionShardDepth (..),
  RedemptionSupplyDatum (..),
  RedemptionSupplyParams (
    RedemptionSupplyParams,
    jitterStrataCount,
    redemptionIncrements,
    redemptionScriptHash,
    redemptionShardDepth,
    tgeWalletAddress
  ),
  RedemptionSupplyRedeemer (..),
  RedemptionSupplyState (Materializing, Subdividing),
  SubdivisionDepth (..),
  TotalIncrements (..),
 )
import Midnight.GlacierDrop.Scripts.ThawingSchedule (
  GenesisTimestamp,
  IncrementsLeft (IncrementsLeft),
  JitterStrataCount (JitterStrataCount),
  JitterStratum (JitterStratum),
  RedemptionIncrementPeriod,
  StarAmount (StarAmount),
  ThawTime (ThawTime),
  ThawingScheduleReduction (
    ThawingScheduleFullyReduced,
    ThawingScheduleNotReduced,
    ThawingSchedulePartiallyReduced
  ),
  initialThaw,
  pastPointFromValidityInterval,
  reduceThawingSchedule,
 )
import PlutusLedgerApi.V1.Value (isZero)
import PlutusLedgerApi.V3 (
  Address (..),
  Credential (PubKeyCredential, ScriptCredential),
  CurrencySymbol,
  Lovelace,
  PubKeyHash (..),
  ScriptHash (..),
  StakingCredential (..),
  TokenName,
  TxInInfo (..),
  TxOut (..),
  Value,
 )
import PlutusPrelude (coerce)
import PlutusTx.List (all)
import PlutusTx.Prelude hiding (error, trace, traceError, traceIfFalse)

-- As per [SPEC]:
-- 1. Inputs:
--   1.1. The transaction should have only one input at the script address - clearly it will be `RedemptionSupply` script.
--   1.2. The script should be in `Subdiving rootHash treeDepth genesisTimestamp` state indicated by the datum.
--   1.3. The script input should contain only ADA and NIGHT tokens.
--   1.4. All the other inputs should contain only ADA.
-- 2. Minting: there should be no minting in the transaction.
-- 3. Outputs:
--   3.1. `treeDepth - redeemer.subdivisionDepth == desiredShardDepth` => all the outputs at the same script address should be in the `Materializing childHash childBitmap genesisTimestamp` state.
--   3.2. The `Materializing` `childBitmap` should be empty - all the bits should be set to 0.
--   3.3. `treeDepth - redeemer.subdivisionDepth < desiredShardDepth` => all the outputs at the same script address should be kept in the `Subdividing childHash childTreeDepth genesisTimestamp` state.
--   3.4. The `Subdividing` `childTreeDepth` should be equal to `treeDepth - redeemer.subdivisionDepth`.
--   3.5. The `genesisTimestamp` should be value preserved.
--   3.6. The sum merkle tree's root hash formed from the continuing outputs with datums containing either `Subdividing` or `Materializing` should be equal the input merkle root hash. The outputs should be processed in order of appearance in the transaction.
--        Please reference [INCOMPLETE SHARDS] note below to understand the process of the computations over incomplete shards.
--   3.7. The Star amount for every output should be derived from the `UTxO`'s `Value`.
--   3.8. Total Lovelace locked across all the continuation outputs should be at least equal to the total Lovelace locked on the input.
--   3.9. The actual depth of the division tree derived from the outputs should equal to the declared `treeDepth` from the redeemer.
--
-- 1.2 is already checked and witnessed by the other arguments: `rootHash`, `merkleTreeDepth`, `genesisTimestamp`.
{-# INLINEABLE subdivide #-}
subdivide
  :: RedemptionShardDepth
  -> SubdivisionDepth
  -> [MerkleHash]
  -> MerkleHash
  -> MerkleTreeDepth
  -> GenesisTimestamp
  -> RedemptionIncrementPeriod
  -> SelfSeparatedTxInfo
  -> Bool
subdivide (RedemptionShardDepth shardDepth) (SubdivisionDepth divisionDepth) emptySubtreeHashes rootHash merkleTreeDepth genesisTimestamp redemptionIncrementPeriod selfSeparatedTxInfo = do
  let SelfSeparatedTxInfo
        (TxInfo{txInfoInputs, txInfoOutputs, txInfoMint})
        _
        ownHash
        stakingCredential
        ownValue = selfSeparatedTxInfo
      othersAreNonScriptAdaOnlyInputs = do
        let step TxInInfo{txInInfoResolved = txOut} = do
              case txOut of
                TxOut (Address (ScriptCredential _) _) _ _ _ ->
                  trace "Input locked by `ScriptCredential` instead of `PubKeyCredential`" False
                TxOut _ inValue _ _ -> traceIfFalse "Non-ADA input" (isAdaOnly inValue)
        all step txInfoInputs
      -- The `noMinting` check is irrelevant to protocol security and just needlessly increases ex-units and thus tx fees.
      -- Furthermore, because we don't need the `txInfoMint` outside of this check, we can even avoid decoding it with a custom TxInfo.
      noMinting = isZero txInfoMint
  -- if we want to preserve this check we can achieve it at near zero cost by comparing `txInfoMint` (in builtin data form) with
  -- the builtin-data encoded empty value:
  --   `emptyValue = BI.mkMap (BI.mkNilPairData BI.unitVal)`
  --   `noMinting = BI.fromOpaque $ BI.equalsData txInfoMint'asData emptyValue`
  othersAreNonScriptAdaOnlyInputs && noMinting && withAnExtraToken ownValue \inputLovelace starSymbol starTokenName starAmount -> do
    -- \^ (1.1 && 1.4) && 2 && 1.3
    withContinuingOutputs ownHash stakingCredential txInfoOutputs \ownOutputsValues _ -> do
      let subtreeDepth = MerkleTreeDepth $ getMerkleTreeDepth merkleTreeDepth - divisionDepth
          starTokenInfo = StarTokenInfo starSymbol starTokenName
          -- Depending on the expected state of the output (either final shards or subtrees which require further subdivision)
          -- we create an own output processing function which is passed to the folding helper.
          withSubtreeInfo =
            \( RedemptionSupplyDatum
                childHash
                childGenesisTimestamp
                childRedemptionIncrementPeriod
                childState
              )
             f -> case compare subtreeDepth shardDepth of
                EQ -> case childState of
                  (Materializing (MaterializationMask childBitmap))
                    -- we substract `3` because the string is in bytes and we want log2(8 * bytes)
                    | ( (decimalLog2 (lengthOfByteString childBitmap))
                          == (getMerkleTreeDepth shardDepth - 3)
                      )
                        && (byteStringToInteger BigEndian childBitmap == 0)
                        && (childGenesisTimestamp == genesisTimestamp)
                        && (childRedemptionIncrementPeriod == redemptionIncrementPeriod) ->
                        f childHash -- 3.1 && 3.2 && 3.5 ; both 3.3 and 3.4 are not applicable
                  _ -> traceError "Invalid state"
                GT -> case childState of
                  (Subdividing childTreeDepth)
                    | (childTreeDepth == subtreeDepth)
                        && (childGenesisTimestamp == genesisTimestamp)
                        && (childRedemptionIncrementPeriod == redemptionIncrementPeriod) ->
                        f childHash -- 3.3 && 3.4 && 3.5 ; both 3.1 and 3.2 are not applicable
                  _ -> traceError "Invalid state"
                LT -> traceError "Subdivision depth is too deep"

      withSubtreesInfo starTokenInfo ownOutputsValues withSubtreeInfo \subtreesInfo -> do
        let lovelaceAtLeastPreserved = subtreesInfo.totalLovelace >= inputLovelace
            (computedRootAmount, computedRootHash, MerkleTreeDepth computedDivisionDepth) = computeRoot (subtreesInfo.subtrees <> fmap (0,) emptySubtreeHashes)
            isDivisionDepthCorrect = computedDivisionDepth == divisionDepth
            isRootHashCorrect = computedRootHash == rootHash
            isRootAmountCorrect = computedRootAmount == starAmount
        -- \^ Verification of the root amount is really redundant - it is included in the root hash itself.
        traceIfFalse "Root hash is incorrect" isRootHashCorrect -- 3.6
          && traceIfFalse "Root amount is incorrect" isRootAmountCorrect -- 3.7
          && traceIfFalse "Total lovelace is not preserved" lovelaceAtLeastPreserved -- 3.8
          && traceIfFalse "Division depth is incorrect" isDivisionDepthCorrect -- 3.9

data StarTokenInfo = StarTokenInfo
  { starSymbol :: CurrencySymbol
  , starTokenName :: TokenName
  }

-- Everything about the datum beside the hash should be validated by the function.
-- This should be rather clear from the below type signature.
type WithSubtreeHash = RedemptionSupplyDatum -> (MerkleHash -> Bool) -> Bool

data SubtreesInfo = SubtreesInfo
  { subtrees :: [(Integer, MerkleHash)]
  , totalLovelace :: Lovelace
  }

{-# INLINEABLE withSubtreesInfo #-}
-- This function does not perform any checks - it just folds the outputs (hashes and star amount).
withSubtreesInfo
  :: StarTokenInfo
  -> [(Value, RedemptionSupplyDatum)]
  -> WithSubtreeHash
  -> (SubtreesInfo -> Bool)
  -> Bool
withSubtreesInfo startTokenInfo continuationsInfo withSubtreeHash f = go id 0 continuationsInfo
  where
    go
      :: ([(Integer, MerkleHash)] -> [(Integer, MerkleHash)])
      -> Lovelace
      -> [(Value, RedemptionSupplyDatum)]
      -> Bool
    go accTrees accLovelace [] = f $ SubtreesInfo (accTrees []) accLovelace
    go accTrees accLovelace ((value, datum) : rest) =
      withSubtreeHash datum \subtreeMerkleHash -> do
        withAnExtraToken value \outputLovelace outputStarSymbol outputStarTokenName outputStarAmount -> do
          let theSameStarToken =
                outputStarSymbol
                  == startTokenInfo.starSymbol
                  && outputStarTokenName
                  == startTokenInfo.starTokenName
          -- \^ The above check is redundant. We know that there is no minting and that on the inputs there is only STAR and ADA.
          theSameStarToken
            && go
              (accTrees . ((outputStarAmount, subtreeMerkleHash) :))
              (outputLovelace + accLovelace)
              rest

{-# INLINEABLE computeRoot #-}
-- Given the bottom row of the merkle tree, computes the root hash and the total amount.
computeRoot :: [(Integer, MerkleHash)] -> (Integer, MerkleHash, MerkleTreeDepth)
computeRoot row = go row 0
  where
    go [] _ = traceError "Empty row"
    go [(rootAmount, rootHash)] rootHeight = (rootAmount, rootHash, MerkleTreeDepth rootHeight)
    go currRow height = go (processRow currRow) (height + 1)

    -- Combines the nodes pair wise producing the parent row
    processRow :: [(Integer, MerkleHash)] -> [(Integer, MerkleHash)]
    processRow [] = []
    processRow [_] = traceError "row length is not a power of 2"
    processRow ((leftAmount, leftHash) : (rightAmount, rightHash) : rest) = do
      let nodeHash = hashNode leftHash leftAmount rightHash rightAmount
          amount = leftAmount + rightAmount
      (amount, nodeHash) : processRow rest

{-# INLINEABLE redemptionSupplyScript #-}
redemptionSupplyScript
  :: RedemptionSupplyParams
  -> ScriptContext Void RedemptionSupplyRedeemer RedemptionSupplyDatum
  -> Bool
redemptionSupplyScript params = do
  let -- A slight optimization with static argument application
      subdivide' = subdivide redemptionShardDepth
  \case
    (ScriptContext _ (SpendingScript _ _ Nothing)) -> trace "in datum" False
    (ScriptContext txInfo (SpendingScript rdmr ownInputRef (Just inDatum))) -> do
      case (rdmr, inDatum) of
        ( RedemptionSupplySubdivide depth emptySubtreeHashes
          , RedemptionSupplyDatum
              rootHash
              genesisTimestamp
              redemptionIncrementPeriod
              (Subdividing merkleTreeDepth)
          ) ->
            withSelfSeparatedTxInfo ownInputRef txInfo \selfSeparatedTxInfo -> do
              subdivide'
                depth
                emptySubtreeHashes
                rootHash
                merkleTreeDepth
                genesisTimestamp
                redemptionIncrementPeriod
                selfSeparatedTxInfo
        ( RedemptionSupplyMaterialize
            materializationInfo@(MaterializationInfo{jitterStratum, amount})
          , RedemptionSupplyDatum
              rootHash
              genesisTimestamp
              redemptionIncrementPeriod
              (Materializing materializationMask)
          ) ->
            withSelfSeparatedTxInfo ownInputRef txInfo
              $ \selfSeparatedTxInfo@(SelfSeparatedTxInfo (TxInfo{txInfoOutputs, txInfoValidRange}) _ _ _ inValue) ->
                withAnExtraToken inValue $ \inAda inExtraPolicyId inExtraToken inExtraAmount ->
                  pastPointFromValidityInterval txInfoValidRange $ \now ->
                    case txInfoOutputs of
                      ownOutput@(TxOut _ outValue _ _) : thawOutput@(TxOut _ thawValue _ _) : outputs ->
                        withMaybeExtraToken outValue $ \outAda outExtra ->
                          withAnExtraToken thawValue $ \_ thawExtraPolicyId thawExtraToken thawExtraAmount ->
                            reduceThawingSchedule
                              redemptionIncrementPeriod
                              now
                              (StarAmount $ amount `divide` totalIncrements)
                              ( initialThaw
                                  (coerce genesisTimestamp)
                                  redemptionIncrementPeriod
                                  (JitterStratum jitterStratum)
                                  (JitterStrataCount jitterStrataCount)
                              )
                              (IncrementsLeft totalIncrements)
                              & \case
                                ThawingScheduleNotReduced -> Nothing
                                ThawingScheduleFullyReduced -> Just Nothing
                                ThawingSchedulePartiallyReduced thawedAmount nextThaw incrementsLeft ->
                                  Just $ Just $ (thawedAmount, nextThaw, incrementsLeft)
                              & \case
                                Nothing -> trace "thaw not reduced" False
                                Just thawSchedule ->
                                  checkRedemptionSupplyMaterialize
                                    rootHash
                                    genesisTimestamp
                                    redemptionIncrementPeriod
                                    materializationMask
                                    materializationInfo
                                    selfSeparatedTxInfo
                                    inAda
                                    inExtraPolicyId
                                    inExtraToken
                                    inExtraAmount
                                    ownOutput
                                    outAda
                                    outExtra
                                    thawOutput
                                    outputs
                                    thawExtraPolicyId
                                    thawExtraToken
                                    thawExtraAmount
                                    thawSchedule
                      _ -> trace "number of outputs" False
        (RedemptionSupplyCollect, _) -> do
          let TxInfo{txInfoOutputs, txInfoMint} = txInfo
          case txInfoOutputs of
            [TxOut address val _ _] ->
              traceIfFalse "Output should be ada only" (isAdaOnly val && isZero txInfoMint)
                && traceIfFalse
                  "Wrong return address"
                  (address == tgeWalletAddress)
            _ -> traceError "Invalid number of outputs"
        (_, _) -> traceError "Invalid redeemer"
  where
    checkRedemptionSupplyMaterialize
      rootHash
      genesisTimestamp
      redemptionIncrementPeriod
      materializationMask
      materializationInfo
      selfSeparatedTxInfo
      inAda
      inExtraPolicyId
      inExtraToken
      inExtraAmount
      ownOutput
      outAda
      outExtra
      thawOutput
      outputs
      thawExtraPolicyId
      thawExtraToken
      thawExtraAmount
      thawSchedule =
        True
          && traceIfFalse
            "proof"
            (checkProof redemptionShardDepth rootHash thawAddress materializationInfo)
          -- redemption supply input
          && traceIfFalse "in extra token" (inExtraToken == newNightTokenName)
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
            )
          -- redemption supply output
          && traceIfFalse
            "redemption supply out address"
            (outAddress == Address (ScriptCredential ownHash) stakingCredential)
          && traceIfFalse "redemption supply out ada" (inAda <= outAda)
          && ( case outExtra of
                Nothing -> traceIfFalse "all star claimed" (0 == inExtraAmount - redemptionTotalAmount)
                Just (outExtraPolicyId, outExtraToken, outExtraAmount) ->
                  True
                    && traceIfFalse
                      "redemption supply out extra policy id"
                      (outExtraPolicyId == inExtraPolicyId)
                    && traceIfFalse
                      "redemption supply out extra token"
                      (outExtraToken == newNightTokenName)
                    && traceIfFalse
                      "redemption supply out extra amount"
                      (outExtraAmount == inExtraAmount - redemptionTotalAmount)
             )
          && traceIfFalse
            "redemption supply out datum"
            ( outDatum
                == serializeDatum
                  ( RedemptionSupplyDatum
                      rootHash
                      genesisTimestamp
                      redemptionIncrementPeriod
                      (Materializing $ markClaim claimIndex materializationMask)
                  )
            )
          -- thaw output
          && traceIfFalse "thaw out extra policy id" (thawExtraPolicyId == inExtraPolicyId)
          && traceIfFalse "thaw out extra token" (thawExtraToken == newNightTokenName)
          && traceIfFalse
            "thaw out extra amount"
            ( thawExtraAmount
                == maybe
                  redemptionTotalAmount
                  (\(StarAmount thawedAmount, _, _) -> thawedAmount)
                  thawSchedule
            )
          -- mint
          && traceIfFalse "mint" (isZero txInfoMint)
          -- redemption output
          && case thawSchedule of
            Nothing ->
              -- TODO[auditors]. we are deliberately not imposing any constraints on the remaining `outputs`. is this safe?
              True
            Just (StarAmount thawedAmount, ThawTime nextThaw, IncrementsLeft incrementsLeft) ->
              case outputs of
                TxOut redemptionAddress redemptionValue redemptionDatum _ : _remainingOutputs ->
                  withAnExtraToken redemptionValue $ \_ redemptionExtraPolicyId redemptionExtraToken redemptionExtraAmount ->
                    True
                      -- TODO[auditors]. we are deliberately not imposing any constraints on the `_remainingOutputs`. is this safe?
                      && traceIfFalse
                        "redemption out address"
                        ( redemptionAddress
                            == Address
                              (ScriptCredential redemptionScriptHash)
                              (addressStakingCredential thawAddress)
                        )
                      && traceIfFalse
                        "redemption out extra policy id"
                        (redemptionExtraPolicyId == inExtraPolicyId)
                      && traceIfFalse
                        "redemption out extra token"
                        (redemptionExtraToken == newNightTokenName)
                      && traceIfFalse
                        "redemption out extra amount"
                        (redemptionExtraAmount == redemptionTotalAmount - thawedAmount)
                      && traceIfFalse
                        "redemption out datum"
                        ( redemptionDatum
                            == serializeDatum
                              ( RedemptionDatum
                                  thawAddress
                                  (redemptionTotalAmount `divide` totalIncrements)
                                  nextThaw
                                  incrementsLeft
                                  redemptionIncrementPeriod
                              )
                        )
                _ -> trace "number of outputs" False
        where
          MaterializationInfo
            { amount = redemptionTotalAmount
            , claimIndex
            } = materializationInfo
          SelfSeparatedTxInfo
            (TxInfo{txInfoInputs, txInfoMint})
            _
            ownHash
            stakingCredential
            _ = selfSeparatedTxInfo
          TxOut outAddress _ outDatum _ = ownOutput
          TxOut thawAddress _ _ _ = thawOutput

    RedemptionSupplyParams
      { redemptionScriptHash
      , jitterStrataCount
      , redemptionShardDepth
      , redemptionIncrements = TotalIncrements totalIncrements
      , tgeWalletAddress
      } =
        params

{-# INLINEABLE checkProof #-}
checkProof
  :: RedemptionShardDepth -> MerkleHash -> Address -> MaterializationInfo -> Bool
checkProof
  (RedemptionShardDepth (MerkleTreeDepth redemptionShardDepth))
  rootHash
  destinationAddress
  MaterializationInfo
    { amount
    , jitterStratum
    , membershipProof
    , claimIndex
    } =
    case mkLeafHash
      destinationAddress
      (coerce amount :: StarAmount)
      (coerce jitterStratum :: JitterStratum) of
      Nothing -> traceIfFalse "Invalid destination address" False
      Just currentHash -> go 0 currentHash amount membershipProof claimIndex
    where
      go lenAcc currentHash _currentAmount [] ix =
        traceIfFalse "Proof too short" (lenAcc == redemptionShardDepth)
          && traceIfFalse "Claim index and proof length mismatch" (ix == 0)
          && traceIfFalse "Root hash mismatch" (currentHash == rootHash)
      go lenAcc currentHash currentAmount ((siblingHash, siblingAmount) : path') ix
        | ix `modulo` 2 == 0 =
            go
              (succ lenAcc)
              (hashNode currentHash currentAmount siblingHash siblingAmount)
              (currentAmount + siblingAmount)
              path'
              (ix `divide` 2)
        | otherwise =
            go
              (succ lenAcc)
              (hashNode siblingHash siblingAmount currentHash currentAmount)
              (siblingAmount + currentAmount)
              path'
              (ix `divide` 2)

-- See Note [Merkle tree schema] in RedemptionSupply.Types
{-# INLINEABLE mkLeafHash #-}
mkLeafHash
  :: Address
  -> StarAmount
  -> JitterStratum
  -> Maybe MerkleHash
mkLeafHash destinationAddress (StarAmount amount) (JitterStratum jitterStratum) = do
  let toCredentialBytes :: Credential -> BuiltinByteString
      toCredentialBytes (PubKeyCredential (PubKeyHash pkh)) = pkh
      toCredentialBytes (ScriptCredential (ScriptHash sh)) = sh

      -- In the case of pointer we should return Nothing.
      toAddressBytes :: Address -> Maybe BuiltinByteString
      toAddressBytes (Address addressCredential stakingCredential) = do
        let credBytes = toCredentialBytes addressCredential
        case stakingCredential of
          Nothing -> Just credBytes
          Just (StakingHash sh) -> Just $ appendByteString credBytes (toCredentialBytes sh)
          Just (StakingPtr{}) -> Nothing

  case toAddressBytes destinationAddress of
    Nothing -> Nothing
    Just addressBytes ->
      Just
        . merkleHash
        . appendByteString (integerToByteString BigEndian 4 jitterStratum)
        . appendByteString (integerToByteString BigEndian 8 amount)
        $ addressBytes

{-# INLINEABLE merkleHash #-}
merkleHash :: BuiltinByteString -> MerkleHash
merkleHash = MerkleHash . sha2_256

{-# INLINEABLE markClaim #-}
markClaim :: Integer -> MaterializationMask -> MaterializationMask
markClaim n (MaterializationMask claimMask)
  -- In this case, bit n is set i.e. the claim has been made
  | readBit claimMask n = traceError "Claim already made"
  -- In this case, bit n is not set i.e. the claim has not been made
  | otherwise =
      MaterializationMask $ writeBits claimMask [n] True

{-# INLINEABLE decimalLog2 #-}
decimalLog2 :: Integer -> Integer
decimalLog2 n
  | n == 0 = traceError "log2: -inf"
  | n == 1 = 0
  | n `modulo` 2 == 0 = 1 + decimalLog2 (n `divide` 2)
  | otherwise = traceError "log2: non-decimal output"
