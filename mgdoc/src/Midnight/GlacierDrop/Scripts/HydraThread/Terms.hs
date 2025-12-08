module Midnight.GlacierDrop.Scripts.HydraThread.Terms where

import Data.Void (Void)
import GHC.ByteOrder (ByteOrder (BigEndian))
import Midnight.GlacierDrop.Api (
  AddressAttributes (..),
  Bip32ChainCode (..),
  BitcoinScriptPubKeyInfo (..),
  BitcoinSigningScheme (..),
  CardanoSigningScheme (..),
  Ed25519PublicKey (..),
  MerkleHash (..),
  PlainMessage (..),
  SchnorrSecp256k1PublicKey (..),
  ScriptContext,
  Secp256k1PublicKey (..),
  Sha512HalfScriptHash (Sha512HalfScriptHash),
  Signature (Signature),
  Star (Star),
  TxInfo (
    TxInfo,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoRedeemers,
    txInfoSignatories
  ),
  VerifiableClaimCredential (..),
 )
import Midnight.GlacierDrop.Scripts.Common (
  intersectionLength,
  isHydraThreadToken,
  mkSpendingScript,
  serializeDatum,
  trace,
  traceError,
  traceIfFalse,
  withAnExtraToken,
 )
import Midnight.GlacierDrop.Scripts.HydraThread.Types (
  ClaimInfo (..),
  HydraThreadClaiming (
    MakeHydraThreadClaiming,
    allotmentsTreeDepth,
    allotmentsTreeRoot,
    alreadyClaimedTrieRoot,
    redemptionsSum
  ),
  HydraThreadDatum (..),
  HydraThreadFinal (MakeHydraThreadFinal),
  HydraThreadParams (..),
  HydraThreadRedeemer (..),
 )
import Midnight.GlacierDrop.Scripts.Night.Types (
  NightMintRedeemer (NightBurnGenesis),
 )
import Midnight.GlacierDrop.Scripts.RedemptionSupply.Terms (merkleHash)
import Midnight.GlacierDrop.Scripts.Supply.Bitcoin.BIP0137 qualified as BIP0137
import Midnight.GlacierDrop.Scripts.Supply.Bitcoin.BIP0322 qualified as BIP0322
import Midnight.GlacierDrop.Scripts.Supply.Cardano qualified as Cardano
import Midnight.GlacierDrop.Scripts.Supply.Cardano.AddressBech32 (AddressBech32)
import Midnight.GlacierDrop.Scripts.Supply.Cardano.AddressBech32 qualified as AddressBech32
import Midnight.GlacierDrop.Scripts.Supply.Ethereum qualified as Ethereum
import Midnight.GlacierDrop.Scripts.Supply.Solana qualified as Solana
import Midnight.GlacierDrop.Scripts.Supply.XRP (Sha512HalfProxy)
import Midnight.GlacierDrop.Scripts.Supply.XRP qualified as XRP
import Midnight.GlacierDrop.Scripts.Supply.XRP qualified as Xrp
import Midnight.GlacierDrop.Scripts.Supply.XRP.SHA512 (sha512Half)
import Midnight.GlacierDrop.Scripts.ThawingSchedule (
  JitterStratum (JitterStratum),
 )
import PlutusLedgerApi.V1.Value (flattenValue, isZero, valueOf)
import PlutusLedgerApi.V3 (
  Address (..),
  Credential (..),
  PubKeyHash (..),
  Redeemer (Redeemer),
  ScriptHash,
  ScriptPurpose (Minting, Rewarding),
  StakingCredential,
  TxOut (TxOut),
  TxOutRef (txOutRefId),
  Value,
 )
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AMap
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.List qualified as List
import PlutusTx.Prelude hiding (error, trace, traceError, traceIfFalse)
import PlutusTx.Show qualified as P
import Refined (makeAscending)

data MerkleLeaf = MerkleLeaf
  { amount :: Integer
  , chainTag :: Integer
  , addressHash :: BuiltinByteString
  }

type ScriptPurposeAsData = BuiltinData

-- | Insert an element in the merkle patricia forestry. This requires a proof of inclusion for the element.
-- The latter can be obtained off-chain from the whole forestry containing the element.
--
-- Returns `False` when
--     * the proof is invalid or
--     * there's already an element in the forestry at the given key.
{-# INLINEABLE trieInsert #-}
trieInsert
  :: BuiltinByteString
  -- ^ key to insert
  -> BuiltinByteString
  -- ^ value to insert
  -> BuiltinData
  -- ^ proof
  -> MerkleHash
  -- ^ old root hash
  -> MerkleHash
  -- ^ new root hash
  -> AMap.Map ScriptPurpose Redeemer
  -- ^ redeemers
  -> ScriptPurposeAsData
  -- ^ purpose
  -> Bool
trieInsert key value proof (MerkleHash rootHash) (MerkleHash newRootHash) redeemers trieInsertScriptPurpose =
  getFFIResult (AMap.toList redeemers)
    == newRootHash
  where
    expectedFields :: BuiltinData
    expectedFields =
      BI.mkList
        $ BI.mkCons
          (BI.mkB rootHash)
          ( BI.mkCons
              (BI.mkB key)
              (BI.mkCons (BI.mkB value) (BI.mkCons proof $ BI.mkNilData BI.unitval))
          )

    verifyInputs :: Redeemer -> BuiltinByteString
    verifyInputs (Redeemer rdmr) =
      let rdmrFields = BI.unsafeDataAsList rdmr
       in if BI.mkList (BI.tail rdmrFields) == expectedFields
            then BI.unsafeDataAsB (BI.head rdmrFields)
            else traceError "Invalid redeemer"

    getFFIResult :: [(ScriptPurpose, Redeemer)] -> BuiltinByteString
    getFFIResult =
      go
      where
        go :: [(ScriptPurpose, Redeemer)] -> BuiltinByteString
        go [] = traceError "trieInsert: FFI withdraw redeemer not found"
        go ((purp, redeemer) : xs) =
          BI.ifThenElse
            ( BI.equalsData
                (PlutusTx.toBuiltinData purp)
                (PlutusTx.toBuiltinData trieInsertScriptPurpose)
            )
            (\_ -> verifyInputs redeemer)
            (\_ -> go xs)
            BI.unitval

-- A helper script which we extracted out:
--  * HydraThread script which included that function was too large to execute on L1
--  * We call sha512Half only during claiming which is on Hydra
--  * Given the above we were able to extract that script and call it through reward proxy
{-# INLINEABLE sha512HalfProxyScript #-}
sha512HalfProxyScript
  :: V3.ScriptContext
  -> Bool
sha512HalfProxyScript (V3.ScriptContext{V3.scriptContextRedeemer = V3.Redeemer hashes}) = do
  let hashes' :: [(BuiltinByteString, BuiltinByteString)]
      hashes' = unsafeFromBuiltinData hashes

      go ((preimage, hash) : hs) = sha512Half preimage == hash && go hs
      go [] = True

  go hashes'

{-# INLINEABLE hydraScript #-}
hydraScript
  :: HydraThreadParams
  -> ScriptContext Void HydraThreadRedeemer HydraThreadDatum
  -> Bool
-- Note: mkSpendingScript uses mkScript that extracts the current input from txInfoInputs
hydraScript = mkSpendingScript . hydraSpend

-- HydraThread Spending Validator
-- Contains 4 different execution paths, differentiated by the redeemer
--
-- 1. HydraThreadGenesis Path
-- 1.1. HydraThreadRedeemer has to be of type HydraThreadGenesis
-- 1.2. HydraThreadToken has to be present in the HydraThread input that is being spent
-- 1.3. Night MintingPolicy has to be present with NightBurnGenesis redeemer
--
-- 2. HydraThreadEnter Path
-- 2.1. HydraThreadRedeemer has to be of type HydraThreadEnter
-- 2.2. Destination address of the first output has to be equal to hydraCommitScriptHash from HydraThreadParams
-- 2.3. There is no minting/burning happening in the transaction
-- 2.4. There needs to be at least *minAuthSignatures* unique signatures from tgeAgentAuthKeys present in HydraThreadParams
--
-- 3. HydraThreadFinalize Path
-- 3.1. HydraThreadRedeemer has to be of type HydraThreadFinalize
-- 3.2. The HydraThread input that is being spent needs to contain ADA + one additional AssetClass
-- 3.3. There is only one transaction output
-- 3.4. The additional AssetClass is HydraThreadToken
-- 3.5. There is exactly one HydraThreadToken in the currently spent input value
-- 3.6. There is only one input in the transaction (the one currently being spent)

-- * Note: mkSpendingScript uses mkScript that removes the current input from txInfoInputs

-- 3.7. Output destination must be the HydraThread Script
-- 3.8. The value of the input must be equal to the output

-- * Note (3.6 + 3.8): Assuming that HydraHead has 0 fees set

-- 3.9. The output datum is changed from HydraThreadClaiming to HydraThreadFinal
-- 3.10. There is no minting/burning happening in the transaction
-- 3.11. There needs to be at least *minAuthSignatures* unique signatures from tgeAgentAuthKeys present in HydraThreadParams
--
-- 4. HydraThreadClaim
-- 4.1. HydraThreadRedeemer has to be of type HydraThreadClaim
-- 4.2. The HydraThread input that is being spent needs to contain ADA + one additional AssetClass
-- 4.3. There is only one transaction output
-- 4.4. Provided signature from Redeemer needs to be valid
-- 4.5. The additional AssetClass is HydraThreadToken
-- 4.6. There is exactly one HydraThreadToken in the currently spent input value
-- 4.7. There is only one input in the transaction (the one currently being spent)

-- * Note: mkSpendingScript uses mkScript that removes the current input from txInfoInputs

-- 4.8. Output destination must be the HydraThread Script
-- 4.9. The value of the input must be equal to the output

-- * Note (4.7 + 4.9): Assuming that HydraHead has 0 fees set

-- 4.10. HydraThreadClaiming Datum must be correctly updated
-- 4.11. There is no minting/burning happening in the transaction
-- 4.12. Provided membership proof in the allotment tree has to be valid
-- 4.13. Claim must not be already present in the trie
{-# INLINEABLE hydraSpend #-}
hydraSpend
  :: HydraThreadParams
  -> TxInfo
  -> TxOutRef
  -> ScriptHash
  -> Maybe StakingCredential
  -> Value
  -> HydraThreadRedeemer
  -> HydraThreadDatum
  -> Bool
hydraSpend _ (TxInfo{txInfoRedeemers}) _ _ _ inValue HydraThreadGenesis (HydraThreadFinal{}) =
  -- 1.1
  case List.find (\(_, t, _) -> isHydraThreadToken t) (flattenValue inValue) of
    Nothing -> trace "hydra thread token" False
    Just (policyId, _, _) -- 1.2
      | Just (Redeemer redeemer) <- AMap.lookup (Minting policyId) txInfoRedeemers
      , Just (NightBurnGenesis{}) <- fromBuiltinData redeemer ->
          True -- 1.3
      | otherwise -> trace "hydra NightBurnGenesis redeemer" False
hydraSpend params txInfo _ _ _ inValue HydraThreadEnter (HydraThreadClaiming{}) =
  withAnExtraToken inValue $ \_inAda inExtraPolicyId inExtraToken inExtraAmount ->
    -- 2.1
    ( case txInfo.txInfoOutputs of
        (TxOut destAddress destValue _ _) : _ ->
          traceIfFalse
            "dest extra token"
            (valueOf destValue inExtraPolicyId inExtraToken == inExtraAmount)
            && traceIfFalse
              "hydra thread output address"
              (addressCredential destAddress == ScriptCredential params.hydraCommitScriptHash) -- 2.2
        _ -> trace "hydra thread enter bad outputs" False
    )
      && traceIfFalse "mint" (isZero txInfo.txInfoMint) -- 2.3
      && traceIfFalse
        "Not authorized"
        ( params.minAuthSignatures
            <= intersectionLength
              params.tgeAgentAuthKeys
              (makeAscending txInfo.txInfoSignatories)
        ) -- 2.4
hydraSpend
  (HydraThreadParams{minAuthSignatures, tgeAgentAuthKeys})
  (TxInfo{txInfoInputs, txInfoOutputs, txInfoMint, txInfoSignatories})
  ownRef
  ownHash
  stakingCredential
  inValue
  (HydraThreadFinalize redemptionsTreeRoot redemptionsTreeDepth) -- 3.1
  (HydraThreadClaiming (MakeHydraThreadClaiming{redemptionsSum})) =
    withAnExtraToken inValue $ \_inAda _inExtraPolicyId inExtraToken inExtraAmount ->
      -- 3.2
      case txInfoOutputs of
        [TxOut outAddress outValue outDatum _] ->
          -- 3.3
          True
            -- hydra thread input
            && traceIfFalse "in extra token" (isHydraThreadToken inExtraToken) -- 3.4.
            && traceIfFalse "in extra amount" (inExtraAmount == 1) -- 3.5
            -- other inputs
            && traceIfFalse "ada inputs" (List.null txInfoInputs) -- 3.6
            -- hydra thread output
            && traceIfFalse
              "out address"
              (outAddress == Address (ScriptCredential ownHash) stakingCredential) -- 3.7
            && traceIfFalse "out value" (outValue == inValue) -- 3.8
            && traceIfFalse
              "out datum"
              ( outDatum
                  == ( serializeDatum
                        $ HydraThreadFinal
                        $ MakeHydraThreadFinal
                          (txOutRefId ownRef)
                          redemptionsTreeRoot
                          redemptionsTreeDepth
                          redemptionsSum
                     )
              ) -- 3.9
              -- mint
            && traceIfFalse "mint" (isZero txInfoMint) -- 3.10
            -- signatures
            && traceIfFalse
              "signatures"
              ( minAuthSignatures
                  <= intersectionLength tgeAgentAuthKeys (makeAscending txInfoSignatories)
              ) -- 3.11
        _ -> trace "number of outputs" False -- 3.3
hydraSpend
  ( HydraThreadParams
      { messagePrefix
      , messageSuffix
      , sha512HalfScriptHash
      , trieInsertCred
      }
    )
  (TxInfo{txInfoInputs, txInfoOutputs, txInfoMint, txInfoRedeemers})
  _
  ownHash
  stakingCredential
  inValue
  ( HydraThreadClaim
      claimInfo@( ClaimInfo
                    { amount
                    , from
                    , to
                    , signature
                    , alreadyClaimedTrieProof
                    , alreadyClaimedTrieRootNew
                    }
                  )
    ) -- 4.1
  ( HydraThreadClaiming
      ( MakeHydraThreadClaiming
          { allotmentsTreeRoot
          , allotmentsTreeDepth
          , alreadyClaimedTrieRoot
          , redemptionsSum
          }
        )
    ) =
    withAnExtraToken inValue $ \_inAda _inExtraPolicyId inExtraToken inExtraAmount ->
      -- 4.2
      case txInfoOutputs of
        [TxOut outAddress outValue outDatum _] ->
          -- 4.3
          traceIfFalse
            "signature"
            ( checkSignature
                messagePrefix
                messageSuffix
                sha512HalfProxy
                from
                to
                amount
                signature -- 4.4
            )
            -- hydra thread input
            && traceIfFalse "in extra token" (isHydraThreadToken inExtraToken) -- 4.5
            && traceIfFalse "in extra amount" (inExtraAmount == 1) -- 4.6
            -- other inputs
            && traceIfFalse "ada inputs" (List.null txInfoInputs) -- 4.7
            -- hydra thread output
            && traceIfFalse
              "out address"
              (outAddress == Address (ScriptCredential ownHash) stakingCredential) -- 4.8
            && traceIfFalse "out value" (outValue == inValue) -- 4.9
            && traceIfFalse
              "out datum"
              ( outDatum
                  == ( serializeDatum
                        $ HydraThreadClaiming
                        $ MakeHydraThreadClaiming
                          { allotmentsTreeRoot
                          , allotmentsTreeDepth
                          , alreadyClaimedTrieRoot = alreadyClaimedTrieRootNew
                          , redemptionsSum = redemptionsSum + amount
                          }
                     )
              ) -- 4.10
              -- mint
            && traceIfFalse "mint" (isZero txInfoMint) -- 4.11
            -- proofs
            && traceIfFalse
              "allotments tree"
              (checkProof allotmentsTreeRoot allotmentsTreeDepth claimInfo) -- 4.12
            && traceIfFalse
              "already claimed trie"
              ( trieInsert
                  (getMerkleHash $ hashLeaf $ mkLeaf from amount)
                  ""
                  alreadyClaimedTrieProof
                  alreadyClaimedTrieRoot
                  alreadyClaimedTrieRootNew
                  txInfoRedeemers
                  (PlutusTx.toBuiltinData (Rewarding trieInsertCred))
              ) -- 4.13
        _ -> trace "number of outputs" False -- 4.3
    where
      sha512HalfProxy = mkSha512HalfProxy sha512HalfScriptHash txInfoRedeemers
hydraSpend _ _ _ _ _ _ _ _ = trace "datum or redeemer" False

{-# INLINEABLE checkProof #-}
checkProof :: MerkleHash -> Integer -> ClaimInfo -> Bool
checkProof allotmentsTreeRoot allotmentsTreeDepth (ClaimInfo{amount, from, allotmentsIndex, allotmentsTreeProof}) =
  go 0 (hashLeaf $ mkLeaf from amount) allotmentsTreeProof allotmentsIndex
  where
    go lenAcc currentHash [] ix =
      traceIfFalse "Proof too short" (lenAcc == allotmentsTreeDepth)
        && traceIfFalse "Claim index and proof length mismatch" (ix == 0)
        && traceIfFalse "Root hash mismatch" (currentHash == allotmentsTreeRoot)
    go lenAcc currentHash (siblingHash : path') ix
      | ix `modulo` 2 == 0 =
          go (succ lenAcc) (hashNode currentHash siblingHash) path' (ix `divide` 2)
      | otherwise =
          go (succ lenAcc) (hashNode siblingHash currentHash) path' (ix `divide` 2)

{-# INLINEABLE makeJitterStratum #-}
makeJitterStratum :: Signature -> Integer -> JitterStratum
makeJitterStratum (Signature signature) jitterStrataCount =
  JitterStratum
    (byteStringToInteger BigEndian signature `modulo` jitterStrataCount)

{-# INLINEABLE checkSignature #-}
checkSignature
  :: BuiltinByteString
  -> BuiltinByteString
  -> Sha512HalfProxy
  -> VerifiableClaimCredential
  -> AddressBech32
  -> Star
  -> Signature
  -> Bool
checkSignature messagePrefix messageSuffix sha512HalfProxy claimFrom claimToAddr star sig = do
  let msg = mkMessage messagePrefix messageSuffix claimToAddr star
  case claimFrom of
    CardanoShelleyCredential cardanoSigningScheme pk ->
      case cardanoSigningScheme of
        FakeTransactionSigningScheme -> Cardano.verifyFakeTxSignature pk msg sig
        CIP8SigningScheme ph ->
          Cardano.verifyCIP8Signature pk ph msg sig
    EthereumCredential pk ->
      Ethereum.verifySignature pk msg sig
    BitcoinCredential bitcoinSigningScheme -> case bitcoinSigningScheme of
      BIP0137 pubKey -> BIP0137.verifySignature pubKey msg sig
      BIP0322 scriptPubKeyInfo sighashFlag -> BIP0322.verifySignature scriptPubKeyInfo sighashFlag msg sig
    SolanaCredential pk sigType ->
      Solana.verifySignature pk msg sigType sig
    BaseCredential pk -> Ethereum.verifySignature pk msg sig
    BnbCredential pk -> Ethereum.verifySignature pk msg sig
    BatCredential pk -> Ethereum.verifySignature pk msg sig
    AvaxCredential pk -> Ethereum.verifySignature pk msg sig
    CardanoByronCredential _ pk _ ->
      Cardano.verifyVerbatimSignature pk msg sig
    XrpCredential pk -> Xrp.verifySignature sha512HalfProxy pk msg sig

{-# INLINEABLE mkMessage #-}
mkMessage
  :: BuiltinByteString -> BuiltinByteString -> AddressBech32 -> Star -> PlainMessage
mkMessage messagePrefix messageSuffix claimToAddr (Star starAmount) = do
  PlainMessage
    $ messagePrefix
    `appendByteString` encodeUtf8 (P.show starAmount)
    `appendByteString` " to "
    `appendByteString` AddressBech32.print claimToAddr
    `appendByteString` messageSuffix

{-# INLINEABLE mkLeaf #-}
mkLeaf :: VerifiableClaimCredential -> Star -> MerkleLeaf
mkLeaf claimFrom (Star amount) = MerkleLeaf{..}
  where
    (chainTag, addressHash) = case claimFrom of
      CardanoShelleyCredential _ (Ed25519PublicKey pk) -> do
        (0x01, blake2b_224 pk)
      CardanoByronCredential
        (AddressAttributes attributes)
        (Ed25519PublicKey pk)
        (Bip32ChainCode chainCode) ->
          ( 0x01
          , blake2b_224
              $ sha3_256
              $ consByteString 0x83
              $ consByteString 0x00
              $ consByteString 0x82
              $ consByteString 0x00
              $ consByteString 0x58
              $ consByteString 0x40
              $ appendByteString pk
              $ appendByteString chainCode attributes
          )
      EthereumCredential (Secp256k1PublicKey pk) ->
        ( 0x02
        , -- `Drop 12 = take last 20` in this case (32 bytes)
          dropByteString 12 $ keccak_256 pk
        )
      BitcoinCredential (BIP0137 pk) ->
        let (PubKeyHash pkh) = BIP0137.hashAnySecp256k1PublicKey pk
         in (0x03, pkh)
      BitcoinCredential (BIP0322 (P2TRPubKey (SchnorrSecp256k1PublicKey pk)) _) ->
        (0x03, pk)
      BitcoinCredential (BIP0322 (P2WPKHPubKey pk) _) ->
        let (PubKeyHash pkh) = BIP0137.hashAnySecp256k1PublicKey pk
         in (0x03, pkh)
      BitcoinCredential (BIP0322 (P2PKHPubKey pk) _) ->
        let (PubKeyHash pkh) = BIP0137.hashAnySecp256k1PublicKey pk
         in (0x03, pkh)
      SolanaCredential (Ed25519PublicKey pk) _ ->
        (0x04, pk)
      BaseCredential (Secp256k1PublicKey pk) ->
        ( 0x05
        , -- Drop 12 = take last 20
          dropByteString 12 $ keccak_256 pk
        )
      BnbCredential (Secp256k1PublicKey pk) ->
        ( 0x06
        , -- Drop 12 = take last 20
          dropByteString 12 $ keccak_256 pk
        )
      BatCredential (Secp256k1PublicKey pk) ->
        ( 0x07
        , -- Drop 12 = take last 20
          dropByteString 12 $ keccak_256 pk
        )
      AvaxCredential (Secp256k1PublicKey pk) ->
        ( 0x08
        , -- Drop 12 = take last 20
          dropByteString 12 $ keccak_256 pk
        )
      XrpCredential pk -> do
        let PubKeyHash pubKeyHashBytes = XRP.hashPublicKey pk
        (0x09, pubKeyHashBytes)

{-# INLINEABLE hashLeaf #-}
hashLeaf :: MerkleLeaf -> MerkleHash
hashLeaf MerkleLeaf{..} =
  merkleHash
    $ appendByteString (integerToByteString BigEndian 8 amount)
    $ consByteString chainTag addressHash

{-# INLINEABLE hashNode #-}
hashNode :: MerkleHash -> MerkleHash -> MerkleHash
hashNode (MerkleHash a) (MerkleHash b)
  -- Edge case (literally) if a shard has an odd number of allotments the
  -- right-most pair of leaves, proofd will report the sibling hash as an empty
  -- string. The rule for merging this node is to pass the left child's hash
  -- up, unchanged.
  | b == "" = MerkleHash a
  | otherwise = merkleHash $ appendByteString a b

type TxInfoRedeemers = AMap.Map ScriptPurpose Redeemer

{-# INLINEABLE mkSha512HalfProxy #-}
mkSha512HalfProxy :: Sha512HalfScriptHash -> TxInfoRedeemers -> Sha512HalfProxy
mkSha512HalfProxy (Sha512HalfScriptHash sha512HalfScriptHash) txInfoRedeemers = do
  let expectedScriptPurpose = Rewarding $ ScriptCredential sha512HalfScriptHash
  case AMap.lookup expectedScriptPurpose txInfoRedeemers of
    Just (Redeemer hashes) -> do
      let hashes' :: [(BuiltinByteString, BuiltinByteString)]
          hashes' = unsafeFromBuiltinData hashes
      \preimage -> snd <$> List.find ((== preimage) . fst) hashes'
    Nothing -> const Nothing
