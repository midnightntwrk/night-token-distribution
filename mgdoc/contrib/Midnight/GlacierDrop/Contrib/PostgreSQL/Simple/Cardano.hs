{-# OPTIONS_GHC -Wno-orphans #-}

module Midnight.GlacierDrop.Contrib.PostgreSQL.Simple.Cardano (
  CBORBytes (..),
  RawBytes (..),
) where

import Cardano.Api (
  Address,
  AsType (..),
  BlockHeader (..),
  BlockNo (..),
  ByronAddr,
  ConwayEra,
  Hash,
  SerialiseAsCBOR (deserialiseFromCBOR, serialiseToCBOR),
  SerialiseAsRawBytes (..),
  ShelleyAddr,
  SlotNo (..),
  Tx,
  TxId,
  TxIx (..),
  deserialiseFromRawBytes,
 )
import Cardano.Api qualified as C
import Cardano.Api.Shelley (HasTypeProxy (..), Hash (..))
import Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeFull', serialize)
import Data.ByteString.Lazy qualified as LBS
import Data.Data (Proxy (..))
import Data.Int (Int64)
import Database.PostgreSQL.Simple hiding (query)
import Database.PostgreSQL.Simple.FromField (FromField (..), conversionError)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.ToField (Action (..), ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Midnight.GlacierDrop.Contrib.Cardano.Api ()
import Midnight.GlacierDrop.Contrib.PostgreSQL.Simple (escapeAndAnnotateByteA)

newtype RawBytes a = RawBytes {getRawBytes :: a}
deriving via
  (Only (RawBytes a))
  instance
    (SerialiseAsRawBytes a) => ToRow (RawBytes a)
instance (SerialiseAsRawBytes a) => FromRow (RawBytes a) where
  fromRow = field
instance (SerialiseAsRawBytes a) => ToField (RawBytes a) where
  toField = EscapeByteA . serialiseToRawBytes . getRawBytes
instance (SerialiseAsRawBytes a) => FromField (RawBytes a) where
  fromField f dat = do
    Binary bytes <- fromField f dat
    case deserialiseFromRawBytes (proxyToAsType $ Proxy @a) bytes of
      Left err -> conversionError $ userError $ show err
      Right a -> pure $ RawBytes a

deriving via (RawBytes (Hash BlockHeader)) instance ToRow (Hash BlockHeader)
deriving via (RawBytes (Hash BlockHeader)) instance ToField (Hash BlockHeader)
deriving via (RawBytes (Hash BlockHeader)) instance FromField (Hash BlockHeader)
deriving via (RawBytes TxId) instance ToRow TxId
deriving via (RawBytes TxId) instance ToField TxId
deriving via (RawBytes TxId) instance FromField TxId
instance FromRow TxId where fromRow = field
instance ToRow BlockHeader where
  toRow (BlockHeader slot hash block) = toRow (block, hash, slot)
deriving via
  (RawBytes (Address ShelleyAddr))
  instance
    ToRow (Address ShelleyAddr)
deriving via
  (RawBytes (Address ShelleyAddr))
  instance
    ToField (Address ShelleyAddr)
deriving via
  (RawBytes (Address ShelleyAddr))
  instance
    FromField (Address ShelleyAddr)
instance FromRow (Address ShelleyAddr) where fromRow = field
deriving via
  (RawBytes (Address ByronAddr))
  instance
    ToRow (Address ByronAddr)
deriving via
  (RawBytes (Address ByronAddr))
  instance
    ToField (Address ByronAddr)
deriving via
  (RawBytes (Address ByronAddr))
  instance
    FromField (Address ByronAddr)

instance FromField (Tx ConwayEra) where
  fromField f dat = do
    Binary cborBytes <- fromField f dat
    case deserialiseFromCBOR (AsTx AsConwayEra) cborBytes of
      Left _ -> undefined
      Right r -> pure r

instance ToField (Tx ConwayEra) where
  toField = toField . Binary . serialiseToCBOR

deriving newtype instance ToField SlotNo
instance ToField BlockNo where
  toField (BlockNo n) = toField @Int $ fromIntegral n

instance FromField BlockNo where
  fromField f dat = BlockNo . fromIntegral <$> fromField @Int f dat

instance FromField SlotNo where
  fromField f dat = SlotNo . fromIntegral <$> fromField @Int64 f dat

newtype CBORBytes a = CBORBytes {getCBORBytes :: a}
deriving via
  (Only (CBORBytes a))
  instance
    (ToCBOR a) => ToRow (CBORBytes a)
instance (FromCBOR a) => FromRow (CBORBytes a) where
  fromRow = field
instance (ToCBOR a) => ToField (CBORBytes a) where
  toField = escapeAndAnnotateByteA . LBS.toStrict . serialize . toCBOR . getCBORBytes
instance (FromCBOR a) => FromField (CBORBytes a) where
  fromField f dat = do
    Binary bytes <- fromField f dat
    case decodeFull' bytes of
      Left err -> conversionError $ userError $ show err
      Right a -> pure $ CBORBytes a

deriving via
  (CBORBytes (C.TxOut C.CtxUTxO C.ConwayEra))
  instance
    FromField (C.TxOut C.CtxUTxO C.ConwayEra)

deriving via
  (CBORBytes (C.TxOut C.CtxUTxO C.ConwayEra))
  instance
    ToField (C.TxOut C.CtxUTxO C.ConwayEra)

deriving newtype instance ToField TxIx

instance FromField TxIx where
  fromField f dat = TxIx . fromIntegral <$> fromField @Int64 f dat
