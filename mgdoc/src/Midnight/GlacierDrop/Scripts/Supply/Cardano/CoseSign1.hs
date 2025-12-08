module Midnight.GlacierDrop.Scripts.Supply.Cardano.CoseSign1 where

import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Control.Monad (replicateM_, unless, void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Midnight.GlacierDrop.Api (
  CIP8ProtectedHeaderInfo (..),
  PlainMessage (..),
  Signature (..),
 )
import Midnight.GlacierDrop.Scripts.Supply.Cardano.AddressBech32 qualified as AddressBech32
import PlutusLedgerApi.V3 qualified as P
import Prelude

-- The following decoding helpers are never to be used on-chain.
-- They allow external tx builders to decode the CBOR structure of a COSE_Sign1
-- so they can extract the protected header and the payload.

-- ```cddl
-- COSE_Sign1 = [
--   protected : bstr .cbor header_map,
--   unprotected : header_map,
--   payload : bstr / nil,
--   signature : bstr
-- ]
-- ```

-- | We skip the unprotected header because it is not relevant for us.
data CoseSign1 = CoseSign1
  { protectedHeader :: CIP8ProtectedHeaderInfo
  -- ^ We keep bytes so we can test mkProtectedHeader
  , payload :: PlainMessage
  , signature :: Signature
  }

type ProtectedHeaderKey = Either Int Text

type ProtectedHeaderValue = Either Int ByteString

-- Decode any CBOR item and discard it
skipItem :: Decoder s ()
skipItem = do
  ty <- CBOR.peekTokenType
  case ty of
    CBOR.TypeUInt -> void CBOR.decodeWord
    CBOR.TypeUInt64 -> void CBOR.decodeWord64
    CBOR.TypeNInt -> void CBOR.decodeInt
    CBOR.TypeNInt64 -> void CBOR.decodeInt64
    CBOR.TypeInteger -> void CBOR.decodeInteger
    CBOR.TypeBytes -> void CBOR.decodeBytes
    CBOR.TypeString -> void CBOR.decodeString
    CBOR.TypeListLen -> CBOR.decodeListLen >>= flip replicateM_ skipItem
    CBOR.TypeMapLen -> CBOR.decodeMapLen >>= \n -> replicateM_ n $ skipItem >> skipItem
    CBOR.TypeTag -> void CBOR.decodeTag >> skipItem
    CBOR.TypeBool -> void CBOR.decodeBool
    CBOR.TypeNull -> void CBOR.decodeNull
    CBOR.TypeSimple -> void CBOR.decodeSimple
    _ -> fail $ "Unexpected token type: " ++ show ty

coseSign1Decoder :: Decoder s CoseSign1
coseSign1Decoder = do
  len <- CBOR.decodeListLen
  if len /= 4
    then fail $ "Expected a list of 4 elements, but got " ++ show len
    else
      CoseSign1
        <$> do
          parseProtectedHeaderInfo <$> CBOR.decodeBytes
        <*> do
          -- Unprotected header
          void $ CBOR.decodeMapLenOrIndef >>= skipMap

          payloadBytes <- fromMaybe "" <$> decodeNullOrBytes
          pure $ PlainMessage $ P.toBuiltin payloadBytes
        <*> do
          Signature . P.toBuiltin <$> CBOR.decodeBytes
  where
    skipMap :: Maybe Int -> Decoder s ()
    skipMap Nothing = skipMapIndef
    skipMap (Just n) = replicateM_ n $ do
      _ <- skipItem -- Skip key
      void skipItem -- Skip value

    -- Skip indefinite length map
    skipMapIndef :: Decoder s ()
    skipMapIndef = do
      end <- CBOR.decodeBreakOr
      unless end $ do
        _ <- skipItem -- Skip key
        _ <- skipItem -- Skip value
        skipMapIndef

    decodeNullOrBytes :: Decoder s (Maybe ByteString)
    decodeNullOrBytes = do
      tkType <- CBOR.peekTokenType
      case tkType of
        CBOR.TypeNull -> CBOR.decodeNull >> return Nothing
        CBOR.TypeBytes -> Just <$> CBOR.decodeBytes
        _ -> fail $ "Expected Null or Bytes, but got: " ++ show tkType

type AddressBytes = ByteString

parseProtectedHeaderInfo :: ByteString -> CIP8ProtectedHeaderInfo
parseProtectedHeaderInfo bs =
  case CBOR.deserialiseFromBytes decoder (LBS.fromStrict bs) of
    Right (remainder, result) ->
      if LBS.null remainder
        then StandardProtectedHeader $ P.toBuiltin result
        else NonStandardProtectedHeader $ P.toBuiltin bs
    Left _ -> NonStandardProtectedHeader $ P.toBuiltin bs
  where
    -- Either 29 or 57 bytes. First byte is header the res are payment and optional stake credentials.
    decoder :: Decoder s ByteString
    decoder = do
      let decodeAddressValue = \case
            Right (addrBytes :: ByteString) -> do
              case AddressBech32.parse $ P.toBuiltin addrBytes of
                Just _ -> pure addrBytes
                Nothing ->
                  fail $
                    "Expected address to be 29 or 57 bytes long but got "
                      ++ show (BS.length addrBytes)
            Left n -> fail $ "Expected address, but got " ++ show n

      len <- CBOR.decodeMapLen
      if len /= 3
        then fail $ "Expected a map of 3 elements, but got " ++ show len
        else do
          expectKey (Left 1)
          alg <- decodeValue
          unless (alg == Left (-8)) $ fail "Expected alg value to be -8"
          expectKey (Left 4)
          address <- decodeValue >>= decodeAddressValue
          expectKey (Right "address")
          address' <- decodeValue >>= decodeAddressValue
          unless (address == address') $
            fail "Expected address and 'address' to be the same"
          pure address

    expectKey :: ProtectedHeaderKey -> Decoder s ()
    expectKey key = do
      decodedKey <- decodeKey
      unless (decodedKey == key) $
        fail $
          "Expected key " ++ show key ++ ", but got " ++ show decodedKey

    decodeKey :: Decoder s ProtectedHeaderKey
    decodeKey = do
      ty <- CBOR.peekTokenType
      case ty of
        CBOR.TypeUInt -> Left <$> CBOR.decodeInt
        CBOR.TypeString -> Right <$> CBOR.decodeString
        _ -> fail $ "Expected Int or String key, but got " ++ show ty

    decodeValue :: Decoder s ProtectedHeaderValue
    decodeValue = do
      ty <- CBOR.peekTokenType
      case ty of
        CBOR.TypeUInt -> Left <$> CBOR.decodeInt
        CBOR.TypeNInt -> Left <$> CBOR.decodeInt
        CBOR.TypeBytes -> Right <$> CBOR.decodeBytes
        _ -> fail $ "Expected Int or ByteString value, but got " ++ show ty

parseCoseSign1 :: ByteString -> Either String CoseSign1
parseCoseSign1 bs = do
  let lbs = LBS.fromStrict bs
  case CBOR.deserialiseFromBytes coseSign1Decoder lbs of
    Right (remainder, result)
      | LBS.null remainder -> Right result
      | otherwise -> Left "Trailing data after COSE_Sign1 structure"
    Left err -> Left $ "Decoding error: " ++ show err

-- ```cddl
-- COSE_Key = {
--  1 : tstr / int, ; kty
--  ? 2 : bstr, ; kid
--  3 : int, ; alg
--  -1 : int, ; crv
--  -2 : bstr ; x (public key)
-- ]
-- ```
--
-- CIP30 Cose_Key keys constrints:
--
-- kty (1) - must be set to OKP (1)
-- kid (2) - Optional, if present must be set to the same value as in the COSE_Sign1 specified above.
-- alg (3) - must be set to EdDSA (-8)
-- crv (-1) - must be set to Ed25519 (6)
-- x (-2) - must be set to the public key bytes of the key used to sign the Sig_structure
--
data CoseKey = CoseKey
  { kid :: Maybe ByteString
  , x :: ByteString
  }

coseKeyDecoder :: Decoder s CoseKey
coseKeyDecoder = do
  len <- CBOR.decodeMapLen
  if len /= 4 && len /= 5
    then fail $ "Expected a map of 4 or 5 elements, but got " ++ show len
    else do
      expectKey 1
      skipItem
      kid <-
        if len == 5
          then do
            expectKey 2
            Just <$> CBOR.decodeBytes
          else pure Nothing
      expectKey 3
      expectInt (-8)
      expectKey (-1)
      expectInt 6
      expectKey (-2)
      CoseKey kid <$> CBOR.decodeBytes
  where
    expectKey :: Int -> Decoder s ()
    expectKey int = do
      k <- CBOR.decodeInt
      unless (k == int) $ fail $ "Expected key " ++ show int ++ ", but got " ++ show k

    expectInt :: Int -> Decoder s ()
    expectInt int = do
      k <- CBOR.decodeInt
      unless (k == int) $ fail $ "Expected key " ++ show int ++ ", but got " ++ show k

parseCoseKey :: ByteString -> Either String CoseKey
parseCoseKey bs = do
  let lbs = LBS.fromStrict bs
  case CBOR.deserialiseFromBytes coseKeyDecoder lbs of
    Right (remainder, result)
      | LBS.null remainder -> Right result
      | otherwise -> Left "Trailing data after COSE_Key structure"
    Left err -> Left $ "Decoding error: " ++ show err
