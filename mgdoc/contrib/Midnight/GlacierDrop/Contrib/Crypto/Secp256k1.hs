module Midnight.GlacierDrop.Contrib.Crypto.Secp256k1 where

import Control.Error (note)
import Crypto.Secp256k1 qualified as Secp256k1
import Crypto.Secp256k1.Internal.ForeignTypes (
  LCtx,
  Msg32,
  PubKey64,
  RecSig65,
  Ret,
  isSuccess,
 )
import Crypto.Secp256k1.Internal.Util (
  unsafePackByteString,
  unsafeUseByteString,
 )
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Char (ord)
import Data.Functor ((<&>))
import Data.Word (Word8)
import Foreign (Bits ((.&.)), Ptr, free, mallocBytes, withForeignPtr)
import Foreign.C (CInt (..))
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import Haskoin qualified as Haskoin
import Midnight.GlacierDrop.Api qualified as Supply
import PlutusLedgerApi.Common qualified as P
import Unsafe.Coerce (unsafeCoerce)

newtype RecoverableSignature = RecoverableSignature ByteString
  deriving (Eq, Show)

newtype Message = Message ByteString
  deriving (Eq, Show)

reverse32s :: ByteString -> ByteString
reverse32s bs = BS.reverse (BS.take 32 bs) <> BS.reverse (BS.drop 32 bs)

reverseCompressed :: ByteString -> ByteString
reverseCompressed bs = BS.take 1 bs <> BS.reverse (BS.drop 1 bs)

toPlutusSecp256k1PubKey :: Secp256k1.PubKey -> Supply.Secp256k1PublicKey
toPlutusSecp256k1PubKey (Secp256k1.PubKey pk) = Supply.Secp256k1PublicKey (P.toBuiltin $ reverse32s pk)

toPlutusAnySecp256k1PubKey :: ByteString -> Supply.AnySecp256k1PublicKey
toPlutusAnySecp256k1PubKey pkBytes =
  if BS.length pkBytes == 33
    then do
      let reversed = reverseCompressed pkBytes
      Supply.AnyCompressedSecp256k1PublicKey $
        Supply.CompressedSecp256k1PublicKey (P.toBuiltin reversed)
    else do
      Supply.AnySecp256k1PublicKey $
        toPlutusSecp256k1PubKey (Secp256k1.PubKey pkBytes)

toPlutusSecp256k1Sig :: Secp256k1.Sig -> Supply.Signature
toPlutusSecp256k1Sig (Secp256k1.Sig sig) = Supply.Signature (P.toBuiltin $ reverse32s sig)

parsePubKeyDER :: ByteString -> Either String Secp256k1.PubKey
parsePubKeyDER pubKeyBytes =
  unsafePerformIO $ Secp256k1.withContext $ \ctx -> do
    pure $
      Haskoin.unmarshal ctx pubKeyBytes <&> \(Haskoin.PublicKey point _) -> point

parseSignatureDER :: ByteString -> Either String Secp256k1.Sig
parseSignatureDER signatureBytes =
  unsafePerformIO $ Secp256k1.withContext $ \ctx ->
    pure $ Haskoin.unmarshal ctx signatureBytes

parseRecoverableSignature
  :: RecoverableSignature
  -> Message
  -> Either String (Secp256k1.Sig, Secp256k1.PubKey)
parseRecoverableSignature (RecoverableSignature recSigBytes) (Message msgBytes) = do
  recSig <- note "Failed to make recSig" $ mkRecSig recSigBytes
  recSig65 <- note "Failed to parse recSig" $ recSigParse recSig
  pubKey <-
    note "Failed to recover public key from recSig65" $
      recover recSig65 (Secp256k1.Msg msgBytes)
  sig <- note "Failed to convert recSig65 to signature" $ convert recSig65
  pure (sig, pubKey)

data RecSig = RecSig
  { header :: Int
  , rAndS :: ByteString
  }
  deriving (Generic)

mkRecSig :: ByteString -> Maybe RecSig
mkRecSig bs
  | BS.length bs == 65 = do
      let (headerBytes, rAndS) = BS.splitAt 1 bs
      header <- case Char8.unpack headerBytes of
        [h] -> pure $ (ord h - 27) .&. 0x03
        _ -> Nothing
      return $ RecSig header rAndS
  | otherwise = Nothing

unsafeByteStringToRecSig65 :: ByteString -> RecSig65
unsafeByteStringToRecSig65 = unsafeCoerce

-- /** Parse a compact ECDSA signature (64 bytes + recovery id).
--  *
--  *  Returns: 1 when the signature could be parsed, 0 otherwise
--  *  Args: ctx:     pointer to a context object
--  *  Out:  sig:     pointer to a signature object
--  *  In:   input64: pointer to a 64-byte compact signature
--  *        recid:   the recovery id (0, 1, 2 or 3)
--  */
-- SECP256K1_API int secp256k1_ecdsa_recoverable_signature_parse_compact(
--     const secp256k1_context *ctx,
--     secp256k1_ecdsa_recoverable_signature *sig,
--     const unsigned char *input64,
--     int recid
-- ) SECP256K1_ARG_NONNULL(1) SECP256K1_ARG_NONNULL(2) SECP256K1_ARG_NONNULL(3);
foreign import ccall safe "secp256k1_recovery.h secp256k1_ecdsa_recoverable_signature_parse_compact"
  ecdsaRecoverableSignatureParseCompact
    :: Ptr LCtx
    -> Ptr ByteString
    -> Ptr Word8
    -> CInt
    -> IO CInt

recSigParse :: RecSig -> Maybe RecSig65
recSigParse (RecSig header rAndS) =
  unsafePerformIO $ Secp256k1.withContext $ \(Secp256k1.Ctx ctx) ->
    withForeignPtr ctx $ \ctxPtr ->
      unsafeUseByteString rAndS $ \(rAndSPtr, _) -> do
        recSigPtr <- mallocBytes 65
        ret <-
          ecdsaRecoverableSignatureParseCompact
            ctxPtr
            recSigPtr
            rAndSPtr
            (fromIntegral header)
        if ret == 1
          then do
            bytes <- unsafePackByteString (recSigPtr, 65)
            pure $ Just (unsafeByteStringToRecSig65 bytes)
          else do
            free recSigPtr
            return Nothing

-- /** Recover an ECDSA public key from a signature.
--  *
--  *  Returns: 1: public key successfully recovered (which guarantees a correct signature).
--  *           0: otherwise.
--  *  Args:    ctx:       pointer to a context object.
--  *  Out:     pubkey:    pointer to the recovered public key.
--  *  In:      sig:       pointer to initialized signature that supports pubkey recovery.
--  *           msghash32: the 32-byte message hash assumed to be signed.
--  */
-- SECP256K1_API SECP256K1_WARN_UNUSED_RESULT int secp256k1_ecdsa_recover(
--     const secp256k1_context *ctx,
--     secp256k1_pubkey *pubkey,
--     const secp256k1_ecdsa_recoverable_signature *sig,
--     const unsigned char *msghash32
foreign import ccall safe "secp256k1_recovery.h secp256k1_ecdsa_recover"
  ecdsaRecover
    :: Ptr LCtx
    -> Ptr PubKey64
    -> Ptr RecSig65
    -> Ptr Msg32
    -> IO Ret

recSig65ToByteString :: RecSig65 -> ByteString
recSig65ToByteString = unsafeCoerce

-- | Recover an ECDSA public key from a signature.
recover :: RecSig65 -> Secp256k1.Msg -> Maybe Secp256k1.PubKey
recover recSig (Secp256k1.Msg msg) =
  unsafePerformIO $ Secp256k1.withContext $ \(Secp256k1.Ctx ctx) ->
    withForeignPtr ctx $ \ctxPtr ->
      unsafeUseByteString (recSig65ToByteString recSig) $ \(recSigPtr, _) ->
        unsafeUseByteString msg $ \(msgPtr, _) -> do
          pubKeyPtr <- mallocBytes 64
          ret <- ecdsaRecover ctxPtr pubKeyPtr recSigPtr msgPtr
          if isSuccess ret
            then do
              pk <- unsafePackByteString (pubKeyPtr, 64)
              return $ Secp256k1.pubKey pk
            else return Nothing

-- /** Convert a recoverable signature into a normal signature.
--  *
--  *  Returns: 1
--  *  Args: ctx:    pointer to a context object.
--  *  Out:  sig:    pointer to a normal signature.
--  *  In:   sigin:  pointer to a recoverable signature.
--  */
-- SECP256K1_API int secp256k1_ecdsa_recoverable_signature_convert(
--     const secp256k1_context *ctx,
--     secp256k1_ecdsa_signature *sig,
--     const secp256k1_ecdsa_recoverable_signature *sigin
-- ) SECP256K1_ARG_NONNULL(1) SECP256K1_ARG_NONNULL(2) SECP256K1_ARG_NONNULL(3);
foreign import ccall safe "secp256k1_recovery.h secp256k1_ecdsa_recoverable_signature_convert"
  ecdsaRecoverableSignatureConvert
    :: Ptr LCtx
    -> Ptr ByteString
    -> Ptr RecSig65
    -> IO CInt

convert :: RecSig65 -> Maybe Secp256k1.Sig
convert recSig =
  unsafePerformIO $ Secp256k1.withContext $ \(Secp256k1.Ctx ctx) ->
    withForeignPtr ctx $ \ctxPtr ->
      unsafeUseByteString (recSig65ToByteString recSig) $ \(recSigPtr, _) -> do
        sigPtr <- mallocBytes 64
        ret <- ecdsaRecoverableSignatureConvert ctxPtr sigPtr recSigPtr
        if ret == 1
          then do
            bytes <- unsafePackByteString (sigPtr, 64)
            pure $ Secp256k1.sig bytes
          else do
            free sigPtr
            return Nothing
