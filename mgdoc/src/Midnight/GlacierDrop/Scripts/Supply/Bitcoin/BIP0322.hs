{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# OPTIONS_GHC -fobject-code #-}

module Midnight.GlacierDrop.Scripts.Supply.Bitcoin.BIP0322 where

import GHC.Generics (Generic)
import Midnight.GlacierDrop.Api (
  BitcoinScriptPubKeyInfo (..),
  CompressedSecp256k1PublicKey (..),
  PlainMessage (..),
  SchnorrSecp256k1PublicKey (..),
  SighashFlag (..),
  Signature (Signature),
  mkCompressedSecp256k1PublicKey,
 )
import Midnight.GlacierDrop.Scripts.Supply.Bitcoin (
  MessageHash (..),
 )
import Midnight.GlacierDrop.Scripts.Supply.Bitcoin.BIP0137 qualified as BIP0137
import PlutusLedgerApi.V3 (
  PubKeyHash (..),
 )
import PlutusTx.Builtins.HasOpaque
import PlutusTx.Prelude

newtype ToSpendTxBytes = ToSpendTxBytes BuiltinByteString
  deriving stock (Generic)
  deriving newtype (ToData, FromData)

-- WARNING: This is the the tx hash value in its original order.
-- We don't need the reversed version of it which is usually
-- called `TxId`.
newtype ToSpendTxId = ToSpendTxId BuiltinByteString
  deriving stock (Generic)
  deriving newtype (ToData, FromData)

sha2_256d :: BuiltinByteString -> BuiltinByteString
sha2_256d bs = sha2_256 $ sha2_256 bs

mkToSpendTxId :: ToSpendTxBytes -> ToSpendTxId
mkToSpendTxId (ToSpendTxBytes tx) = ToSpendTxId $ sha2_256d tx

newtype Tag = Tag BuiltinByteString
  deriving stock (Generic)
  deriving newtype (ToData, FromData)

mkTaggedHash :: Tag -> BuiltinByteString -> BuiltinByteString
mkTaggedHash (Tag tag) payload = do
  let tagHash = sha2_256 tag
  sha2_256 $ tagHash `appendByteString` tagHash `appendByteString` payload

mkMessageHash :: PlainMessage -> MessageHash
mkMessageHash (PlainMessage msg) = do
  let tag = Tag "BIP0322-signed-message"
  MessageHash $ mkTaggedHash tag msg

{-# INLINEABLE mkP2TRPubKey #-}
mkP2TRPubKey :: SchnorrSecp256k1PublicKey -> BuiltinByteString
mkP2TRPubKey = do
  let codePrefix = stringToBuiltinByteStringHex "5120"
  \(SchnorrSecp256k1PublicKey pubKey) ->
    codePrefix `appendByteString` pubKey

-- {-# INLINABLE mkP2WPKHPubKey #-}
mkP2WPKHPubKey :: PubKeyHash -> BuiltinByteString
mkP2WPKHPubKey = do
  let codePrefix = stringToBuiltinByteStringHex "0014"
  \(PubKeyHash pubKeyHash) ->
    codePrefix `appendByteString` pubKeyHash

-- {-# INLINABLE mkP2PKHPubKey #-}
mkP2PKHPubKey :: PubKeyHash -> BuiltinByteString
mkP2PKHPubKey = do
  let codePrefix = stringToBuiltinByteStringHex "76a914"
      codeSuffix = stringToBuiltinByteStringHex "88ac"
  \(PubKeyHash pubKeyHash) ->
    codePrefix `appendByteString` pubKeyHash `appendByteString` codeSuffix

-- {-# INLINEABLE mkP2TRPubKeySegment #-}
mkP2TRPubKeySegment :: SchnorrSecp256k1PublicKey -> BuiltinByteString
mkP2TRPubKeySegment = do
  let len = stringToBuiltinByteStringHex "22"
  \pubKey -> do
    len `appendByteString` mkP2TRPubKey pubKey

-- {-# INLINEABLE mkP2WPKHPubKeySegment #-}
mkP2WPKHPubKeySegment :: PubKeyHash -> BuiltinByteString
mkP2WPKHPubKeySegment = do
  let len = stringToBuiltinByteStringHex "16"
  \pubKeyHash ->
    len `appendByteString` mkP2WPKHPubKey pubKeyHash

-- {-# INLINEABLE mkP2PKHPubKeySegment #-}
mkP2PKHPubKeySegment :: PubKeyHash -> BuiltinByteString
mkP2PKHPubKeySegment = do
  let len = stringToBuiltinByteStringHex "19"
  \pubKeyHash ->
    len `appendByteString` mkP2PKHPubKey pubKeyHash

{-# INLINEABLE mkScriptPubKeySegment #-}
mkScriptPubKeySegment :: BitcoinScriptPubKeyInfo -> BuiltinByteString
mkScriptPubKeySegment = \case
  P2TRPubKey pubKey -> mkP2TRPubKeySegment pubKey
  P2WPKHPubKey pubKey -> mkP2WPKHPubKeySegment (BIP0137.hashAnySecp256k1PublicKey pubKey)
  P2PKHPubKey pubKey -> mkP2PKHPubKeySegment (BIP0137.hashAnySecp256k1PublicKey pubKey)

{-# INLINEABLE mkToSpendTx #-}
mkToSpendTx :: BitcoinScriptPubKeyInfo -> PlainMessage -> ToSpendTxBytes
mkToSpendTx = do
  let -- `to_spend` transaction (https://github.com/bitcoin/bips/blob/master/bip-0322.mediawiki#full):
      --    nVersion = 0
      --    nLockTime = 0
      --    vin[0].prevout.hash = 0000...000
      --    vin[0].prevout.n = 0xFFFFFFFF
      --    vin[0].nSequence = 0
      txBytes1 =
        stringToBuiltinByteStringHex
          "00000000010000000000000000000000000000000000000000000000000000000000000000ffffffff220020"
      --    vin[0].scriptSig = OP_0 PUSH32[ message_hash ]
      --    vin[0].scriptWitness = []
      --    vout[0].nValue = 0
      txBytes2 = stringToBuiltinByteStringHex "00000000010000000000000000"
      --    vout[0].scriptPubKey = message_challenge -- to be proven (public) key script.
      txBytes3 = stringToBuiltinByteStringHex "00000000"

  -- Example tx body:
  -- 00000000010000000000000000000000000000000000000000000000000000000000000000ffffffff220020
  -- 9ee0a4968d735503964f236daae4d4bea391e1eaa62d81f843bf58dbb7712dde
  -- 0000000001000000000000000016
  -- 00149a66d6fa724e18c9665467ad10b6f9fce0605aa5
  -- 00000000
  \scriptPubKey msg -> do
    let MessageHash messageHash = mkMessageHash msg
    ToSpendTxBytes
      $ txBytes1
      `appendByteString` messageHash
      `appendByteString` txBytes2
      `appendByteString` mkScriptPubKeySegment scriptPubKey
      `appendByteString` txBytes3

newtype HashForWitness = HashForWitness BuiltinByteString
  deriving stock (Generic)
  deriving newtype (ToData, FromData)

-- Given the `to_sign` transaction:
--
--   nVersion = 0 or (FULL format only) as appropriate (e.g. 2, for time locks)
--   nLockTime = 0 or (FULL format only) as appropriate (for time locks)
--   vin[0].prevout.hash = to_spend.txid
--   vin[0].prevout.n = 0
--   vin[0].nSequence = 0 or (FULL format only) as appropriate (for time locks)
--   vin[0].scriptWitness = message_signature
--   vout[0].nValue = 0
--   vout[0].scriptPubKey = OP_RETURN-
--
-- We are constructing the hash which should be signed by the private key to create a witness:
{-# INLINEABLE mkBufferForWitness #-}
mkBufferForWitness
  :: BitcoinScriptPubKeyInfo -> SighashFlag -> ToSpendTxId -> BuiltinByteString
mkBufferForWitness = do
  let -- _0x is handy helper which is `NOINLINE` and has to be statically evaluated by the
      -- compiler - this is enforced by `INLINE` pragma.
      _8bitsOf0 = stringToBuiltinByteStringHex "00"
      _32bitsOf0 = stringToBuiltinByteStringHex "00000000"
      _64bitsOf0 = stringToBuiltinByteStringHex "0000000000000000"

      version = _32bitsOf0
      inputIndex = _32bitsOf0

      hashOutputs_sha2_256d =
        stringToBuiltinByteStringHex
          "0e8538c66c434675836e2198aceb9a0bb62f3778a505eba907d843d312712b6d"
      hashOutputs_sha2_256 =
        stringToBuiltinByteStringHex
          "cd496407aff71ffb5742f4f51efeac926c9f84eb51c391b5f2049bd74d9b4a02"

      hashAmounts =
        stringToBuiltinByteStringHex
          "af5570f5a1810b7af78caf4bc70a660f0df51e42baf91d4de5b2328de0e83dfc"
      hashSequence_sha2_256d =
        stringToBuiltinByteStringHex
          "8cb9012517c817fead650287d61bdd9c68803b6bf9c64133dcab3e65b5a50cb9"
      hashSequence_sha2_256 =
        stringToBuiltinByteStringHex
          "df3f619804a92fdb4057192dc43dd748ea778adc52bc498ce80524c014b81119"

      lockTime = _32bitsOf0
      _SIGHASH_ALL_32 = stringToBuiltinByteStringHex "01000000"
      _SIGHASH_ALL_8 = stringToBuiltinByteStringHex "01"

      _SIGHASH_DEFAULT_32 = stringToBuiltinByteStringHex "00000000"
      _SIGHASH_DEFAULT_8 = stringToBuiltinByteStringHex "00"

      mkBufferForP2PKHWitness pubKeyHash sighashFlag txId = do
        let prevOutsHash = sha2_256d $ txId `appendByteString` inputIndex
            inputSequence = _32bitsOf0
            inputValue = _64bitsOf0
            sighashFlag32 = case sighashFlag of
              SIGHASH_ALL -> _SIGHASH_ALL_32
              SIGHASH_DEFAULT -> _SIGHASH_DEFAULT_32
        version
          `appendByteString` prevOutsHash
          `appendByteString` hashSequence_sha2_256d
          `appendByteString` txId
          `appendByteString` inputIndex
          `appendByteString` mkP2PKHPubKeySegment pubKeyHash
          `appendByteString` inputValue
          `appendByteString` inputSequence
          `appendByteString` hashOutputs_sha2_256d
          `appendByteString` lockTime
          `appendByteString` sighashFlag32

  \scriptPubKey sighashFlag (ToSpendTxId txId) -> do
    case scriptPubKey of
      -- It can be surprising but both P2WPKH and P2PKH uses P2PKH witness buffer construction
      -- https://github.com/bitcoinjs/bitcoinjs-lib/blob/1a9119b53bcea4b83a6aa8b948f0e6370209b1b4/ts_src/psbt.ts#L1654
      P2WPKHPubKey pubKey ->
        mkBufferForP2PKHWitness
          (BIP0137.hashAnySecp256k1PublicKey pubKey)
          sighashFlag
          txId
      P2PKHPubKey pubKey ->
        mkBufferForP2PKHWitness
          (BIP0137.hashAnySecp256k1PublicKey pubKey)
          sighashFlag
          txId
      P2TRPubKey pubKey -> do
        let prevOutsHash = sha2_256 $ txId `appendByteString` inputIndex
            hashScriptPubKeys = sha2_256 $ mkP2TRPubKeySegment pubKey
            -- Extra zero because of:
            -- https://github.com/bitcoin/bips/blob/master/bip-0341.mediawiki#cite_note-19
            sigHashEpoch = _8bitsOf0
            spendType = _8bitsOf0
            sighashFlag8 = case sighashFlag of
              SIGHASH_ALL -> _SIGHASH_ALL_8
              SIGHASH_DEFAULT -> _SIGHASH_DEFAULT_8

        sigHashEpoch
          `appendByteString` sighashFlag8
          `appendByteString` version
          `appendByteString` lockTime
          `appendByteString` prevOutsHash
          `appendByteString` hashAmounts
          `appendByteString` hashScriptPubKeys
          `appendByteString` hashSequence_sha2_256
          `appendByteString` hashOutputs_sha2_256
          `appendByteString` spendType
          `appendByteString` inputIndex

{-# INLINEABLE mkHashForWitness #-}
mkHashForWitness
  :: BitcoinScriptPubKeyInfo -> SighashFlag -> ToSpendTxId -> HashForWitness
mkHashForWitness scriptPubKey@(P2TRPubKey _) sighashFlag txId = do
  let tag = Tag "TapSighash"
      bytes = mkBufferForWitness scriptPubKey sighashFlag txId
  HashForWitness $ mkTaggedHash tag bytes
mkHashForWitness scriptPubKey sighashFlag txId =
  HashForWitness
    $ sha2_256d
    $ mkBufferForWitness scriptPubKey sighashFlag txId

{-# INLINEABLE verifySignature #-}
verifySignature
  :: BitcoinScriptPubKeyInfo
  -> SighashFlag
  -> PlainMessage
  -> Signature
  -> Bool
verifySignature scriptPubKeyInfo sighashFlag plainMessage (Signature signature) = do
  let toSpendTxId = mkToSpendTxId $ mkToSpendTx scriptPubKeyInfo plainMessage
      HashForWitness hashForWitness = mkHashForWitness scriptPubKeyInfo sighashFlag toSpendTxId
  case scriptPubKeyInfo of
    P2TRPubKey (SchnorrSecp256k1PublicKey pubKeyBytes) -> verifySchnorrSecp256k1Signature pubKeyBytes hashForWitness signature
    P2PKHPubKey anySecp256k1PublicKey ->
      verifyEcdsaSecp256k1Signature
        ( getCompressedSecp256k1PublicKey
            . mkCompressedSecp256k1PublicKey
            $ anySecp256k1PublicKey
        )
        hashForWitness
        signature
    P2WPKHPubKey anySecp256k1PublicKey ->
      verifyEcdsaSecp256k1Signature
        ( getCompressedSecp256k1PublicKey
            . mkCompressedSecp256k1PublicKey
            $ anySecp256k1PublicKey
        )
        hashForWitness
        signature
