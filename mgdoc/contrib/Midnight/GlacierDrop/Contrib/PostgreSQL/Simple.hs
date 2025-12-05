module Midnight.GlacierDrop.Contrib.PostgreSQL.Simple (
  escapeAndAnnotateByteA,
  querySingleRow,
  querySingleRowThrowing,
  querySingleValue,
  querySingleValueThrowing,
  Jsonb (..),
  AnnotatedBinary (..),
)
where

import Data.Aeson as Aeson
import Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Database.PostgreSQL.Simple (
  Connection,
  FromRow,
  Only (..),
  Query,
  ToRow,
  query,
  query_,
 )
import Database.PostgreSQL.Simple.FromField (
  FromField (fromField),
  conversionError,
 )
import Database.PostgreSQL.Simple.ToField (Action (..), ToField (..))

querySingleRow
  :: (FromRow a)
  => Connection
  -> Query
  -> IO (Maybe a)
querySingleRow conn q = do
  Database.PostgreSQL.Simple.query_ conn q >>= \case
    [res] -> pure $ Just res
    _ -> pure Nothing

querySingleRowThrowing
  :: (FromRow a)
  => Connection
  -> Query
  -> IO a
querySingleRowThrowing conn q = do
  querySingleRow conn q >>= \case
    Just res -> pure res
    Nothing -> error "querySingleRowThrowing: expected one result"

querySingleValue
  :: (ToRow a)
  => (FromField b)
  => Connection
  -> Query
  -> a
  -> IO (Maybe b)
querySingleValue conn q params = do
  Database.PostgreSQL.Simple.query conn q params >>= \case
    [Database.PostgreSQL.Simple.Only res] -> pure $ Just res
    _ -> pure Nothing

querySingleValueThrowing
  :: (ToRow a)
  => (FromField b)
  => Connection
  -> Query
  -> a
  -> IO b
querySingleValueThrowing conn q params = do
  querySingleValue conn q params >>= \case
    Just res -> pure res
    Nothing -> error "querySingleValueThrowing: expected one result"

newtype Jsonb a = Jsonb a

instance (ToJSON a) => ToField (Jsonb a) where
  toField (Jsonb a) =
    let bs = LBS.toStrict (encode a)
     in Many
          [ Plain "("
          , Escape bs
          , Plain ")::jsonb"
          ]

instance (FromJSON a) => FromField (Jsonb a) where
  fromField f dat = do
    v <- fromField f dat
    case Aeson.parseEither Aeson.parseJSON v of
      Left err -> conversionError $ Aeson.AesonException err
      Right a -> pure . Jsonb $ a

escapeAndAnnotateByteA :: ByteString -> Action
escapeAndAnnotateByteA bs =
  Many
    [ Plain "("
    , EscapeByteA bs
    , Plain ")::bytea"
    ]

-- | A wrapper type used to annotate with `bytea` in postgesql calls.
--   It's needed because the PGArray creation calls are wrongly parsed
--   as `text[]` instead of `bytea[]`. Annotation fixes it.
newtype AnnotatedBinary = AnnotatedBinary {getAnnotatedBytea :: ByteString}
  deriving (Show, Eq)

instance ToField AnnotatedBinary where
  toField = escapeAndAnnotateByteA . getAnnotatedBytea

-- -- Use this wrapper only for types which serialize to a `bytea`
-- newtype PGByteaArray a = PGByteaArray { fromPGByteaArray :: [a] }
--
-- instance ToField a => ToField (PGByteaArray a) where
--   toField (PGByteaArray xs) =
--     case xs of
--       [] -> Plain (byteString "'{}'::bytea[]")
--       _  -> Many $
--             [ Plain (byteString "ARRAY[") ] ++
--             intersperse (Plain (char8 ',')) (map toField xs) ++
--             [ Plain (byteString "]::bytea[]") ]
