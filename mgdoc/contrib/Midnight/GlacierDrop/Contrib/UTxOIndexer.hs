module Midnight.GlacierDrop.Contrib.UTxOIndexer where

import Cardano.Binary (serialize, toCBOR, unsafeDeserialize)
import Crypto.Hash.SHA256 as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.Internal qualified as Pg
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Midnight.GlacierDrop.Contrib.Cardano.Api (AUTxO)
import Midnight.GlacierDrop.Contrib.Data.Aeson (HexEncoded (..))

import Cardano.Api (
  ConwayEra,
  FromJSON,
  ToJSON,
  TxIn,
 )
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Database.PostgreSQL.Simple.ToRow (ToRow (toRow))

newtype UTxOHash = UTxOHash ByteString
  deriving newtype (Eq, Ord)

deriving via
  HexEncoded
  instance
    ToJSON UTxOHash

deriving via
  HexEncoded
  instance
    FromJSON UTxOHash

deriving via
  (Binary ByteString)
  instance
    ToField UTxOHash

deriving via
  (Binary ByteString)
  instance
    FromField UTxOHash

newtype UTxORef = UTxORef ByteString
  deriving newtype (Eq, Ord, Show)

deriving via
  HexEncoded
  instance
    ToJSON UTxORef

deriving via
  HexEncoded
  instance
    FromJSON UTxORef

deriving via
  (Binary ByteString)
  instance
    ToField UTxORef

instance FromRow UTxORef where
  fromRow = field

instance ToRow UTxORef where
  toRow = toRow

deriving via
  (Binary ByteString)
  instance
    FromField UTxORef

mkUTxORef :: TxIn -> UTxORef
mkUTxORef = UTxORef . BS.toStrict . serialize . toCBOR

uTxORefToTxIn :: UTxORef -> TxIn
uTxORefToTxIn (UTxORef bs) = unsafeDeserialize $ BS.fromStrict bs

mkUTxOHash :: AUTxO ConwayEra -> UTxOHash
mkUTxOHash =
  UTxOHash . SHA256.hash . BS.toStrict . serialize . toCBOR
