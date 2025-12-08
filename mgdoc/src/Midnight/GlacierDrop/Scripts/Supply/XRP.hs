{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# OPTIONS_GHC -fobject-code #-}

-- | XRP Signature Verification Module for GlacierDrop
module Midnight.GlacierDrop.Scripts.Supply.XRP where

import GHC.ByteOrder (ByteOrder (BigEndian))
import Midnight.GlacierDrop.Api (
  Ed25519PublicKey (..),
  Ed25519orSecp256k1PublicKey (..),
  PlainMessage (..),
  Secp256k1PublicKey (..),
  Signature (..),
 )
import PlutusLedgerApi.V3 (PubKeyHash (..))
import PlutusTx.Builtins qualified as B
import PlutusTx.Builtins.HasOpaque
import PlutusTx.Prelude

type Sha512HalfProxy = BuiltinByteString -> Maybe BuiltinByteString

stxPrefix :: BuiltinByteString
stxPrefix = stringToBuiltinByteStringHex "53545800"

fieldHeaderSigningPubKey :: BuiltinByteString
fieldHeaderSigningPubKey = stringToBuiltinByteStringHex "7321"

fieldHeaderAccount :: BuiltinByteString
fieldHeaderAccount = stringToBuiltinByteStringHex "8114"

fieldHeaderMemos :: BuiltinByteString
fieldHeaderMemos = stringToBuiltinByteStringHex "F9EA7D"

objectEndMarker :: BuiltinByteString
objectEndMarker = stringToBuiltinByteStringHex "E1"

transactionEndMarker :: BuiltinByteString
transactionEndMarker = stringToBuiltinByteStringHex "F1"

{-# INLINEABLE memoLengthBytes #-}
memoLengthBytes :: PlainMessage -> BuiltinByteString
memoLengthBytes (PlainMessage message) = do
  let memoLength = lengthOfByteString message
  if memoLength < 193
    then integerToByteString BigEndian 0 memoLength
    else
      if memoLength - 193 <= 255
        then
          stringToBuiltinByteStringHex "C1"
            `appendByteString` integerToByteString BigEndian 0 (memoLength - 193)
        else traceError "too long memo (448)"

{-# INLINEABLE mkFullMessageXumm #-}
mkFullMessageXumm
  :: PlainMessage
  -> Ed25519orSecp256k1PublicKey
  -> BuiltinByteString
mkFullMessageXumm message pubKey@(Ed25519orSecp256k1PublicKey pubKeyBytes) = do
  let PubKeyHash pubKeyHash = hashPublicKey pubKey
      pubKeyBytesRaw =
        if isEd25519PublicKey pubKey
          then stringToBuiltinByteStringHex "ED" <> pubKeyBytes
          else pubKeyBytes
  stxPrefix
    `appendByteString` fieldHeaderSigningPubKey
    `appendByteString` pubKeyBytesRaw
    `appendByteString` fieldHeaderAccount
    `appendByteString` pubKeyHash
    `appendByteString` fieldHeaderMemos
    `appendByteString` memoLengthBytes message
    `appendByteString` getPlainMessage message
    `appendByteString` objectEndMarker
    `appendByteString` transactionEndMarker

{-# INLINEABLE verifySignature #-}
verifySignature
  :: Sha512HalfProxy
  -> Ed25519orSecp256k1PublicKey
  -> PlainMessage
  -> Signature
  -> Bool
verifySignature sha512Proxy pk@(Ed25519orSecp256k1PublicKey pubKey) plainMsg sig =
  if isEd25519PublicKey pk
    then verifySignatureEd25519 (Ed25519PublicKey pubKey) plainMsg sig
    else verifySignatureSecp256k1 sha512Proxy (Secp256k1PublicKey pubKey) plainMsg sig

{-# INLINEABLE isEd25519PublicKey #-}
isEd25519PublicKey :: Ed25519orSecp256k1PublicKey -> Bool
isEd25519PublicKey (Ed25519orSecp256k1PublicKey pk) = lengthOfByteString pk == 32

{-# INLINEABLE verifySignatureSecp256k1 #-}
verifySignatureSecp256k1
  :: Sha512HalfProxy
  -> Secp256k1PublicKey
  -> PlainMessage
  -> Signature
  -> Bool
verifySignatureSecp256k1 sha512HalfProxy (Secp256k1PublicKey pubKeyBytes) plainMsg (Signature sig) = do
  let msgBytes =
        mkFullMessageXumm
          plainMsg
          (Ed25519orSecp256k1PublicKey pubKeyBytes)
  case sha512HalfProxy msgBytes of
    Just msgHash ->
      traceIfFalse "Signature verification failed"
        $ B.verifyEcdsaSecp256k1Signature pubKeyBytes msgHash sig
    Nothing ->
      traceIfFalse "Sha512Half lookup failed" False

{-# INLINEABLE verifySignatureEd25519 #-}
verifySignatureEd25519 :: Ed25519PublicKey -> PlainMessage -> Signature -> Bool
verifySignatureEd25519 (Ed25519PublicKey pubKeyBytes) plainMsg (Signature sig) = do
  let msgBytes = mkFullMessageXumm plainMsg (Ed25519orSecp256k1PublicKey pubKeyBytes)
  B.verifyEd25519Signature pubKeyBytes msgBytes sig

--
{-# INLINEABLE hashXrpEd25519PublicKey #-}
hashXrpEd25519PublicKey :: Ed25519PublicKey -> PubKeyHash
hashXrpEd25519PublicKey (Ed25519PublicKey pubKey) =
  PubKeyHash
    $ B.ripemd_160 (B.sha2_256 $ stringToBuiltinByteStringHex "ED" <> pubKey)

{-# INLINEABLE hashXrpSecp256k1PublicKey #-}
hashXrpSecp256k1PublicKey :: Secp256k1PublicKey -> PubKeyHash
hashXrpSecp256k1PublicKey (Secp256k1PublicKey pubKey) =
  PubKeyHash $ B.ripemd_160 (B.sha2_256 pubKey)

hashPublicKey :: Ed25519orSecp256k1PublicKey -> PubKeyHash
hashPublicKey pk@(Ed25519orSecp256k1PublicKey pubKey) =
  if isEd25519PublicKey pk
    then hashXrpEd25519PublicKey (Ed25519PublicKey pubKey)
    else hashXrpSecp256k1PublicKey (Secp256k1PublicKey pubKey)

-- A helper used in off-chain code which indicates if a given public key requires hashing
mkMessageHashPreimage
  :: Ed25519orSecp256k1PublicKey -> PlainMessage -> Maybe BuiltinByteString
mkMessageHashPreimage pk plainMsg =
  if isEd25519PublicKey pk
    then Nothing
    else Just $ mkFullMessageXumm plainMsg pk
