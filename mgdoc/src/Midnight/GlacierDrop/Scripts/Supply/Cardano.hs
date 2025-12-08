{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# OPTIONS_GHC -fobject-code #-}

module Midnight.GlacierDrop.Scripts.Supply.Cardano (
  mkSigStructure,
  mkProtectedHeader,
  verifyCIP8Signature,
  verifyVerbatimSignature,
  verifyFakeTxSignature,
  mkFakeTxMessage,
  module AddressBech32,
)
where

import Midnight.GlacierDrop.Scripts.Supply.CBOR (
  CBOR (CBOR),
  encodeSmallByteString,
  unCBOR,
 )
import Midnight.GlacierDrop.Scripts.Supply.Cardano.AddressBech32 as AddressBech32 (
  AddressBech32 (..),
  HeaderType (..),
  Network (..),
 )

import Midnight.GlacierDrop.Api (
  CIP8ProtectedHeaderInfo (..),
  Ed25519PublicKey (..),
  PlainMessage (..),
  Signature (Signature),
 )
import Midnight.GlacierDrop.Scripts.Common (trace, traceIfFalse)
import PlutusTx.Builtins qualified as EBI
import PlutusTx.Builtins.HasOpaque
import PlutusTx.Prelude hiding (trace, traceIfFalse)

-- | This is customized encoding of protected body:
-- * wasn't able to find external source for ED25519 -8` algorithm encoding
-- * wallets put pubKeyHash into `kid` (key identifier) field which is part of the RFC spec
-- * "address" is custom CIP-0008  extension
--
-- CDDL:
-- protected-header = {
--   1 => -8,
--   4 => bstr ; kid (key identifier)
--   "address" => bstr .size 57
-- }
--
-- Example:
-- ```
-- a3                                     #   map(3)
--    01                                  #   unsigned(1)
--    27                                  #   negative(-8)
--    04                                  #   unsigned(4)
--    58 39                               #   bytes(57)
--       01623785d8ceb1a7798cf4144bcf6278 #     "\x01b7\x85\xd8\xce\xb1\xa7y\x8c\xf4\x14K\xcfbx"
--       cf4571ce634cd61840eb6542bb2facb6 #     "\xcfEq\xcecL\xd6\x18@\xebeB\xbb/\xac\xb6"
--       f93266a995eb8cfd89745a473c0eab1e #     "\xf92f\xa9\x95\xeb\x8c\xfd\x89tZG<\x0e\xab\x1e"
--       c2c6e689b7eff634f1               #     "\xc2\xc6\xe6\x89\xb7\xef\xf64\xf1"
--    67                                  #   text(7)
--       61646472657373                   #     "address"
--    58 39                               #   bytes(57)
--       01623785d8ceb1a7798cf4144bcf6278 #     "\x01b7\x85\xd8\xce\xb1\xa7y\x8c\xf4\x14K\xcfbx"
--       cf4571ce634cd61840eb6542bb2facb6 #     "\xcfEq\xcecL\xd6\x18@\xebeB\xbb/\xac\xb6"
--       f93266a995eb8cfd89745a473c0eab1e #     "\xf92f\xa9\x95\xeb\x8c\xfd\x89tZG<\x0e\xab\x1e"
--       c2c6e689b7eff634f1               #     "\xc2\xc6\xe6\x89\xb7\xef\xf64\xf1"-
-- ```
{-# INLINEABLE mkProtectedHeader #-}
mkProtectedHeader :: BuiltinByteString -> Maybe BuiltinByteString
mkProtectedHeader addressBytes = do
  let prefix = stringToBuiltinByteStringHex "a3012704"
  case encodeSmallByteString addressBytes of
    Just (CBOR pubKeyHashCBOR) -> do
      let addressKey = stringToBuiltinByteStringHex "6761646472657373"
      Just
        $ prefix
        `appendByteString` pubKeyHashCBOR
        `appendByteString` addressKey
        `appendByteString` pubKeyHashCBOR
    _ -> Nothing

-- https://github.com/cardano-foundation/CIPs/tree/master/CIP-0008#signing-and-verification-target-format
-- CDDL:
-- Sig_structure = [
--   context : "Signature" / "Signature1" / "CounterSignature",    ; explained below
--   body_protected : empty_or_serialized_map,                     ; protected from layer 1
--   ? sign_protected : empty_or_serialized_map,                   ; not present in Sign1 case
--   external_aad : bstr,                                          ; explanation below
--   payload : bstr
-- ]
--
-- Example:
-- 84                                     # array(4)
--    6a                                  #   text(10)
--       5369676e617475726531             #     "Signature1"
--    58 82                               #   bytes(130)
--       a301270458390064e244f5bdc0cc990b #     "\xa3\x01\'\x04X9\x00d\xe2D\xf5\xbd\xc0\xcc\x99\x0b"
--       d742c7376832affd6bcebce47a50548d #     "\xd7B\xc77h2\xaf\xfdk\xce\xbc\xe4zPT\x8d"
--       6a5769652f2a431766d557fa18075c25 #     "jWie/*C\x17f\xd5W\xfa\x18\x07\\%"
--       542fa9972396067ea8959be4384a5667 #     "T/\xa9\x97#\x96\x06~\xa8\x95\x9b\xe48JVg"
--       6164647265737358390064e244f5bdc0 #     "addressX9\x00d\xe2D\xf5\xbd\xc0"
--       cc990bd742c7376832affd6bcebce47a #     "\xcc\x99\x0b\xd7B\xc77h2\xaf\xfdk\xce\xbc\xe4z"
--       50548d6a5769652f2a431766d557fa18 #     "PT\x8djWie/*C\x17f\xd5W\xfa\x18"
--       075c25542fa9972396067ea8959be438 #     "\x07\\%T/\xa9\x97#\x96\x06~\xa8\x95\x9b\xe48"
--       4a56                             #     "JV"
--    40                                  #   bytes(0)
--                                        #     ""
--    43                                  #   bytes(3)
--       626172                           #     "bar"
--
--  If a given protected header is "not a standard one" then it is provided as a value.
{-# INLINEABLE mkSigStructure #-}
mkSigStructure
  :: CIP8ProtectedHeaderInfo -> PlainMessage -> Maybe BuiltinByteString
mkSigStructure = do
  let -- An array header together with first constant value
      prefix = stringToBuiltinByteStringHex "846a5369676e617475726531"
      -- An empty byte string
      emtpyBytes = stringToBuiltinByteStringHex "40"
  \protectedHeaderInfo (PlainMessage messageBytes) -> do
    payloadCBOR <- encodeSmallByteString messageBytes
    protectedHeaderCBOR <-
      encodeSmallByteString =<< case protectedHeaderInfo of
        StandardProtectedHeader pubKeyHash -> mkProtectedHeader pubKeyHash
        NonStandardProtectedHeader ph -> Just ph
    pure
      $ prefix
      `appendByteString` unCBOR protectedHeaderCBOR
      `appendByteString` emtpyBytes
      `appendByteString` unCBOR payloadCBOR

-- | Efficient CIP-0008 signature verification
-- |
-- | Background:
-- | * The `Cose_Sign1` structure is an array of 4 items:
-- |   protected header, unprotected header, payload, and signature.
-- | * The actual bytes which are signed consist of some constant CBOR wrapper around the protected header and the payload.
-- | * Protected header (from many tested wallets) is some constant CBOR wrapper around duplicated public key hash which
-- |   we construct.
-- | * The protected header structure is not a standard so we still allow to provide it as a value.
-- |
-- | Verification:
-- | * We have to reconstruct the signed bytes.
-- | * We don't need the whole `Cose_Sign1` for that.
-- | * We need payload (message) and signature which we are checking.
-- | * We can either construct protected header if it has standard structure or we can accept it as a value.
{-# INLINEABLE verifyCIP8Signature #-}
verifyCIP8Signature
  :: Ed25519PublicKey
  -> CIP8ProtectedHeaderInfo
  -> PlainMessage
  -> Signature
  -> Bool
verifyCIP8Signature (Ed25519PublicKey pubKey) protectedHeaderInfo plainMessage (Signature signature) = do
  case mkSigStructure protectedHeaderInfo plainMessage of
    Just sigStructure ->
      traceIfFalse "Incorrect signature"
        $ verifyEd25519Signature pubKey sigStructure signature
    _ -> trace "Cannot encode message" False

{-# INLINEABLE verifyVerbatimSignature #-}
verifyVerbatimSignature :: Ed25519PublicKey -> PlainMessage -> Signature -> Bool
verifyVerbatimSignature (Ed25519PublicKey pubKey) (PlainMessage message) (Signature signature) =
  traceIfFalse "Incorrect signature"
    $ verifyEd25519Signature pubKey message signature

-- offchain the transaction btyes will be prefixed by 84
-- so for the message:
-- a00d90102818258200000000000000000000000000000000000000000000000000000000000000000000180020003010758201b58e0524c57d1a79b545073a960bfcee74e26bffa27cc79d22bf412999439110ed9010281581cf120862e979a660a8a068485a930e78de8a5804aff7612895b1f7725
-- offchain we need to add:
--  `84` to the beginning of the above tx body
--  `A0F5A10078` to the end of the above tx body which is the witness set and the script validity flag
--  final step is to add the actual message bytes (prefixed by the length of the message) after the witness set and the script validity flag
-- so offchain the actual transaction bytes that get signed are:
-- "84" <> fakeTxMessageBody <> "A0F5A10078" <> (integerToByteString BigEndian (lengthOfByteString plainMessageBytes) 1) <> plainMessageBytes
{-# INLINEABLE mkFakeTxMessage #-}
mkFakeTxMessage :: Ed25519PublicKey -> PlainMessage -> BuiltinByteString
mkFakeTxMessage (Ed25519PublicKey pubKey) (PlainMessage message) =
  -- The prefix hex bytes a transaction with:
  -- exactly one input with a tx-out-ref of:
  --    a tx-id consistint of 32 zeros
  --    a tx-output-index of 0
  -- zeros for fees
  -- empty outputs
  -- zero expiration
  -- 5820 indicates the bytes and the length
  -- 7 indicates that it is the metdata field prefix
  -- and the leading bytes for transaction metadata
  let prefixBytes =
        stringToBuiltinByteStringHex
          "A700D9010281825820000000000000000000000000000000000000000000000000000000000000000000018002000301075820"
      -- A1                                   # map(1)
      -- 00                                # unsigned(0)
      --
      -- A100 is a map of 1 with a tag 0
      -- indicating that the metadata is of the structure:
      -- {0: MESSAGE_HERE}
      metadataMessagePrefixBytes = stringToBuiltinByteStringHex "A100"
      metadataDefiniteListLengthBytes = divide (lengthOfByteString message + 63) 64
      -- 128 is the integer representation of "80" hex which stands for the definite list tag
      metadataDefiniteListPrefix =
        EBI.integerToByteString EBI.BigEndian 1 $ 128 + metadataDefiniteListLengthBytes
      metadataMessageDefiniteList =
        calculateMetadataDefiniteList (lengthOfByteString message) message
      metadataMessage =
        blake2b_256
          $ metadataMessagePrefixBytes
          `appendByteString` metadataDefiniteListPrefix
          `appendByteString` metadataMessageDefiniteList
      -- required signer
      -- 258 tag is non-empty set tag
      -- 581C indicates the length of required signers bytes
      requiredSignersPrefix = stringToBuiltinByteStringHex "0ED9010281581C"
      networdId = stringToBuiltinByteStringHex "0F01"
   in blake2b_256
        $ prefixBytes
        `appendByteString` metadataMessage
        `appendByteString` requiredSignersPrefix
        `appendByteString` (blake2b_224 pubKey)
        `appendByteString` networdId
  where
    -- \| Because the plain message we are signing:
    -- "STAR 11 to addr_test1vp3r0pwce6c6w7vv7s2yhnmz0r852uwwvdxdvxzqadj59wclh7n74 accepting <TODO:TERMS_AND_CONDITIONS_HASH>"
    -- is greater than 64 bytes, the cbor format of the transaction limits individual bytestrings to 64 bytes so
    -- we must chunk the message into a definite list where each element is 64 bytes (or less for the last element)
    -- If the length of a given chunk is less than or equal to 23 bytes then the length byte is between 60 to 77
    -- otherwise 78 indicates that the following byte determines the length of the chunk.
    -- In cbor if the byte ends in 8 or f cbor has special rules.
    --   Because we are working with text the special rule for the cbor tag is that:
    --     60 represents the starting point for representing the string of length less than 23.
    calculateMetadataDefiniteList
      :: Integer -> BuiltinByteString -> BuiltinByteString
    calculateMetadataDefiniteList metadataLength metadataBytes =
      if metadataLength <= 0
        then ""
        else
          let chunk = takeByteString 64 metadataBytes
              prefixByte =
                if lengthOfByteString chunk <= 23
                  then EBI.integerToByteString EBI.BigEndian 1 $ lengthOfByteString chunk + 96
                  else
                    appendByteString (stringToBuiltinByteStringHex "78")
                      $ EBI.integerToByteString EBI.BigEndian 1 (lengthOfByteString chunk)
              cborChunk = prefixByte `appendByteString` chunk
              next =
                calculateMetadataDefiniteList
                  (metadataLength - 64)
                  (dropByteString 64 metadataBytes)
           in cborChunk `appendByteString` next

{-# INLINEABLE verifyFakeTxSignature #-}
verifyFakeTxSignature :: Ed25519PublicKey -> PlainMessage -> Signature -> Bool
verifyFakeTxSignature (Ed25519PublicKey pubKey) (PlainMessage message) (Signature signature) =
  let fakeTxMessage = mkFakeTxMessage (Ed25519PublicKey pubKey) (PlainMessage message)
   in traceIfFalse "Incorrect signature"
        $ verifyEd25519Signature pubKey fakeTxMessage signature
