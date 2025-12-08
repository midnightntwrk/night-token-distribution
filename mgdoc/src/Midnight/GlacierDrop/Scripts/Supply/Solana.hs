{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# OPTIONS_GHC -fobject-code #-}

module Midnight.GlacierDrop.Scripts.Supply.Solana where

import GHC.ByteOrder (ByteOrder (LittleEndian))
import Midnight.GlacierDrop.Api (
  Ed25519PublicKey (..),
  PlainMessage (..),
  Signature (..),
  SolanaSignature (..),
 )
import qualified PlutusTx.Builtins as Builtins
import PlutusTx.Builtins.HasOpaque
import PlutusTx.Prelude

-- Constants for Standardized format
signingDomainSpecifier :: BuiltinByteString
signingDomainSpecifier = stringToBuiltinByteStringHex "ff" `appendByteString` "solana offchain"

headerVersion :: BuiltinByteString
headerVersion = stringToBuiltinByteStringHex "00"

applicationDomain :: BuiltinByteString
applicationDomain =
  stringToBuiltinByteStringHex
    "0000000000000000000000000000000000000000000000000000000000000000"

messageFormat :: BuiltinByteString
messageFormat = stringToBuiltinByteStringHex "00"

signerCount :: BuiltinByteString
signerCount = stringToBuiltinByteStringHex "01"

-- Message Preamble:
--  * sc = signer count * 32 - in our case just 32
--
--  | Field              | Offset | Len | Description
--  |--------------------|--------|-----|--------------------------------
--  | Signing Domain     | 0x00   |  16 | "\xffsolana offchain"
--  | Header version     | 0x10   |   1 | Only 0 is supported
--  | Application domain | 0x11   |  32 | Application specific identifier - we sould request 0 during signing.
--  | Message format     | 0x31   |   1 | We use 0 here (Restricted ASCII with Hardware Wallet Support)
--  | Signer count       | 0x32   |   1 | Number of signers committing to the message. An 8-bit, unsigned integer. We use 1 here.
--  | Signers            | 0x33   |  sc | ed25519 public keys of signers
--  | Message length     | 0x33+sc|   2 | Length of message in bytes. A 16-bit, unsigned, little-endian integer.
--
-- `solana-cli` uses subset of fields: domain, header version, message format, and message length fields
{-# INLINEABLE constructSolanaCLIPreamble #-}
constructSolanaCLIPreamble :: PlainMessage -> BuiltinByteString
constructSolanaCLIPreamble = do
  let constantPrefix =
        signingDomainSpecifier
          <> headerVersion
          <> messageFormat
  \(PlainMessage msg) ->
    constantPrefix
      `appendByteString` integerToByteString LittleEndian 2 (lengthOfByteString msg)

{-# INLINEABLE constructPreamble #-}
constructPreamble :: PlainMessage -> Ed25519PublicKey -> BuiltinByteString
constructPreamble = do
  let constantPrefix =
        signingDomainSpecifier
          <> headerVersion
          <> applicationDomain
          <> messageFormat
          <> signerCount
  \(PlainMessage msg) (Ed25519PublicKey pubKey) ->
    constantPrefix
      `appendByteString` pubKey
      `appendByteString` integerToByteString LittleEndian 2 (lengthOfByteString msg)

{-# INLINEABLE verifySignature #-}
verifySignature
  :: Ed25519PublicKey -> PlainMessage -> SolanaSignature -> Signature -> Bool
verifySignature pubKey@(Ed25519PublicKey pubKeyBytes) msg@(PlainMessage msgBytes) sigType (Signature sigBytes) =
  case sigType of
    SolanaVerbatim ->
      Builtins.verifyEd25519Signature pubKeyBytes msgBytes sigBytes
    SolanaCLIFormat -> do
      let preamble = constructSolanaCLIPreamble msg
          fullMessage = preamble <> msgBytes
      Builtins.verifyEd25519Signature pubKeyBytes fullMessage sigBytes
    SolanaOffchainMessage -> do
      let preamble = constructPreamble msg pubKey
          fullMessage = preamble <> msgBytes
      Builtins.verifyEd25519Signature pubKeyBytes fullMessage sigBytes
