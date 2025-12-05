{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# OPTIONS_GHC -fobject-code #-}

module Midnight.GlacierDrop.Scripts.Supply.Bitcoin.BIP0137 (
  hashAnySecp256k1PublicKey,
  mkMessageHash,
  verifySignature,
  mkFullMessage,
  encodeVarInt,
)
where

import GHC.ByteOrder (ByteOrder (..))
import Midnight.GlacierDrop.Scripts.Supply.Bitcoin.Types (
  MessageHash (..),
 )

import PlutusTx.Builtins qualified as B

import Midnight.GlacierDrop.Api (
  AnySecp256k1PublicKey (..),
  CompressedSecp256k1PublicKey (..),
  PlainMessage (..),
  Secp256k1PublicKey (..),
  Signature (..),
  getCompressedSecp256k1PublicKey,
  mkCompressedSecp256k1PublicKey,
 )
import Midnight.GlacierDrop.Scripts.Common (traceError)
import PlutusLedgerApi.V3 (PubKeyHash (..))
import PlutusTx.Builtins qualified as BI
import PlutusTx.Prelude hiding (traceError)

-- 2 ** 64 - 1
maxUInt64 :: Integer
maxUInt64 = 18_446_744_073_709_551_616 - 1

encodeVarInt :: Integer -> BuiltinByteString
encodeVarInt n
  | n < 0 = traceError "Negative varint"
  | n < 0xfd -- single byte
    =
      BI.integerToByteString LittleEndian 1 n
  | n <= 0xffff -- prefix 0xfd + 2-bytes LE
    =
      BI.appendByteString
        (BI.integerToByteString LittleEndian 1 0xfd)
        (BI.integerToByteString LittleEndian 2 n)
  | n <= 0xffffffff -- prefix 0xfe + 4-bytes LE
    =
      BI.appendByteString
        (BI.integerToByteString LittleEndian 1 0xfe)
        (BI.integerToByteString LittleEndian 4 n)
  | n <= maxUInt64 -- prefix 0xff + 8-bytes LE
    =
      BI.appendByteString
        (BI.integerToByteString LittleEndian 1 0xff)
        (BI.integerToByteString LittleEndian 8 n)
  | otherwise = traceError "VarInt overflow"

{-# INLINEABLE mkFullMessage #-}
mkFullMessage :: PlainMessage -> BuiltinByteString
mkFullMessage = do
  let messagePrefix :: BuiltinByteString
      messagePrefix = "Bitcoin Signed Message:\n"
      messagePrefixLength = integerToByteString BigEndian 0 (lengthOfByteString messagePrefix)

  \(PlainMessage message) -> do
    let messageLength = encodeVarInt (lengthOfByteString message)
    messagePrefixLength
      `appendByteString` messagePrefix
      `appendByteString` messageLength
      `appendByteString` message

{-# INLINEABLE mkMessageHash #-}
mkMessageHash :: PlainMessage -> MessageHash
mkMessageHash msg = do
  let fullMessage = mkFullMessage msg
  MessageHash $ sha2_256 $ sha2_256 fullMessage

{-# INLINEABLE verifySignature #-}
verifySignature :: AnySecp256k1PublicKey -> PlainMessage -> Signature -> Bool
verifySignature anySecp256k1PublicKey plainMessage (Signature signature) = do
  let MessageHash messageHash = mkMessageHash plainMessage
      pubKeyBytes =
        getCompressedSecp256k1PublicKey
          . mkCompressedSecp256k1PublicKey
          $ anySecp256k1PublicKey
  B.verifyEcdsaSecp256k1Signature pubKeyBytes messageHash signature

hashCompressedPubKey :: CompressedSecp256k1PublicKey -> PubKeyHash
hashCompressedPubKey (CompressedSecp256k1PublicKey bs) = PubKeyHash $ B.ripemd_160 (B.sha2_256 bs)

hashPublicKey :: Secp256k1PublicKey -> PubKeyHash
hashPublicKey (Secp256k1PublicKey bs) = PubKeyHash $ B.ripemd_160 (B.sha2_256 $ "\x04" `appendByteString` bs)

hashAnySecp256k1PublicKey :: AnySecp256k1PublicKey -> PubKeyHash
hashAnySecp256k1PublicKey = \case
  AnyCompressedSecp256k1PublicKey compressed -> hashCompressedPubKey compressed
  AnySecp256k1PublicKey uncompressed -> hashPublicKey $ uncompressed
