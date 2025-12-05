{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# OPTIONS_GHC -fobject-code #-}

module Midnight.GlacierDrop.Scripts.Supply.Ethereum where

import GHC.ByteOrder (ByteOrder (BigEndian))
import GHC.Generics (Generic)
import PlutusLedgerApi.V3 (
  PubKeyHash (..),
 )
import PlutusTx.Builtins qualified as B

import Midnight.GlacierDrop.Api (
  CompressedSecp256k1PublicKey (CompressedSecp256k1PublicKey),
  PlainMessage (..),
  Secp256k1PublicKey (..),
  Signature (..),
  compressSecp256k1PublicKey,
 )
import PlutusTx.Prelude

import PlutusTx.Show (Show (show))

import PlutusTx.Builtins.HasOpaque
import Prelude qualified as Haskell

newtype MessageHash = MessageHash BuiltinByteString
  deriving stock (Generic, Haskell.Show, Haskell.Eq, Haskell.Ord)
  deriving newtype (ToData, FromData)

mkFullMessage :: PlainMessage -> BuiltinByteString
mkFullMessage = do
  let messagePrefix :: BuiltinByteString
      messagePrefix = (stringToBuiltinByteStringUtf8 "Ethereum Signed Message:\n")

      messagePrefixLength :: BuiltinByteString
      messagePrefixLength = integerToByteString BigEndian 0 (lengthOfByteString messagePrefix)

  \(PlainMessage message) -> do
    -- Yes, Ethereum mixes the length encoding :-)
    let messageLength = encodeUtf8 $ show (lengthOfByteString message)
    messagePrefixLength
      `appendByteString` messagePrefix
      `appendByteString` messageLength
      `appendByteString` message

mkMessageHash :: PlainMessage -> MessageHash
mkMessageHash msg = do
  let fullMessage = mkFullMessage msg
  MessageHash $ keccak_256 fullMessage

verifySignature :: Secp256k1PublicKey -> PlainMessage -> Signature -> Bool
verifySignature pubKey plainMessage (Signature signature) = do
  let MessageHash messageHash = mkMessageHash plainMessage
      CompressedSecp256k1PublicKey pubKey' = compressSecp256k1PublicKey pubKey
  B.verifyEcdsaSecp256k1Signature pubKey' messageHash signature

verifyPubKeyHash :: Secp256k1PublicKey -> PubKeyHash -> Bool
verifyPubKeyHash pubKey pubKeyHash = do
  let pubKeyHashComputed = hashEthereumPublicKey pubKey
  pubKeyHashComputed == pubKeyHash

hashEthereumPublicKey :: Secp256k1PublicKey -> PubKeyHash
hashEthereumPublicKey (Secp256k1PublicKey pubKey) =
  PubKeyHash $ sliceByteString 12 20 $ keccak_256 pubKey
