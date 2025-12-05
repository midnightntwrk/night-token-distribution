module Midnight.GlacierDrop.Contrib.MpfCli (
  MpfCliInsertBatch (..),
  MpfCliPath (..),
  MpfCliDbPath (..),
  MpfCliKey (..),
  MpfCliValue (..),
  MpfCliResponse (..),
  MpfRootHash (..),
  InsertNodeResponse (..),
  ProofResponse (..),
  MpfClient (..),
  emptyRootHash,
  mkMpfClient,
) where

import Control.Exception (IOException, evaluate, try)
import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  eitherDecode,
  object,
  withObject,
  (.:),
  (.:?),
  (.=),
 )
import Data.Aeson qualified as A
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Text qualified as Text
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Midnight.GlacierDrop.Contrib.Data.Aeson (HexEncoded (..), encodeHex)
import PlutusLedgerApi.Common (
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
 )
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import System.Process (readProcess)

-- | Path to the mpf-cli executable
newtype MpfCliPath = MpfCliPath {unMpfCliPath :: FilePath}
  deriving newtype (Show, Eq)

newtype BuiltinByteString_ = BuiltinByteString_ ByteString

instance ToData BuiltinByteString_ where
  toBuiltinData (BuiltinByteString_ bs) = toBuiltinData (BuiltinByteString bs)

instance FromData BuiltinByteString_ where
  fromBuiltinData bd = case fromBuiltinData bd of
    Just (BuiltinByteString bs) -> Just (BuiltinByteString_ bs)
    _ -> Nothing

newtype MpfRootHash = MpfRootHash {unMpfRootHash :: ByteString}
  deriving newtype (Show, Eq)

deriving via
  BuiltinByteString_
  instance
    ToData MpfRootHash

deriving via
  BuiltinByteString_
  instance
    FromData MpfRootHash

deriving via
  HexEncoded
  instance
    ToJSON MpfRootHash

deriving via
  HexEncoded
  instance
    FromJSON MpfRootHash

emptyRootHash :: MpfRootHash
emptyRootHash = MpfRootHash $ BS.pack $ replicate 32 0x00

-- | Path to the mpf-cli database
newtype MpfCliDbPath = MpfCliDbPath {unMpfCliDbPath :: FilePath}
  deriving newtype (Show, Eq)

newtype MpfCliKey = MpfCliKey {unMpfCliKey :: ByteString}
  deriving newtype (Show, Eq)

deriving via
  BuiltinByteString_
  instance
    ToData MpfCliKey

deriving via
  BuiltinByteString_
  instance
    FromData MpfCliKey

deriving via
  HexEncoded
  instance
    FromJSON MpfCliKey

deriving via
  HexEncoded
  instance
    ToJSON MpfCliKey

newtype MpfCliValue = MpfCliValue {unMpfCliValue :: ByteString}
  deriving newtype (Show, Eq)

deriving via
  BuiltinByteString_
  instance
    ToData MpfCliValue

deriving via
  BuiltinByteString_
  instance
    FromData MpfCliValue

deriving via
  HexEncoded
  instance
    FromJSON MpfCliValue

deriving via
  HexEncoded
  instance
    ToJSON MpfCliValue

data MpfCliResponse a
  = MpfCliResponseSuccess a
  | MpfCliResponseError String
  deriving (Show)

instance (FromJSON a) => FromJSON (MpfCliResponse a) where
  parseJSON = withObject "MpfCliResponse" $ \v -> do
    success <- v .: "success"
    if success
      then MpfCliResponseSuccess <$> v .: "data"
      else MpfCliResponseError <$> v .: "error"

instance (ToJSON a) => ToJSON (MpfCliResponse a) where
  toJSON (MpfCliResponseSuccess data') = object ["success" .= True, "data" .= data']
  toJSON (MpfCliResponseError errorMsg) = object ["success" .= False, "error" .= errorMsg]

data ProofResponse = ProofResponse
  { verified :: Bool
  , proofCBOR :: ByteString
  }
  deriving (Show)

instance FromJSON ProofResponse where
  parseJSON = withObject "ProofResponse" $ \v -> do
    verified <- v .: "verified"
    HexEncoded proofCBOR <- v .: "proofCBOR"
    return ProofResponse{..}

instance ToJSON ProofResponse where
  toJSON (ProofResponse verified proofCBOR) =
    object ["verified" .= verified, "proofCBOR" .= HexEncoded proofCBOR]

data InsertNodeResponse = InsertNodeResponse
  { newHash :: MpfRootHash
  , prevHash :: Maybe MpfRootHash
  , proofResp :: ProofResponse
  }
  deriving (Show)

instance ToJSON InsertNodeResponse where
  toJSON InsertNodeResponse{..} =
    object
      [ "newHash" .= newHash
      , "prevHash" .= prevHash
      , "proofResp" .= proofResp
      ]

instance FromJSON InsertNodeResponse where
  parseJSON = withObject "InsertNodeResponse" $ \v -> do
    newHash <- v .: "newHash"
    prevHash <- v .:? "prevHash"
    proofResp <- v .: "proofResp"
    return InsertNodeResponse{..}

encodeHexStr :: ByteString -> String
encodeHexStr = Text.unpack . encodeHex

-- | Insert a new node
_insertNode
  :: MpfCliPath
  -> MpfCliDbPath
  -> MpfCliKey
  -> MpfCliValue
  -> IO (Either String (MpfCliResponse InsertNodeResponse))
_insertNode cliPath dbPath (MpfCliKey key) (MpfCliValue value) =
  fst
    <$> runMpfCliJson
      cliPath
      dbPath
      ["insert", "--key", encodeHexStr key, "--value", encodeHexStr value]

newtype MpfCliInsertBatch = MpfCliInsertBatch
  {unInsertBatch :: [(MpfCliKey, MpfCliValue)]}
  deriving (Show)

instance ToJSON MpfCliInsertBatch where
  toJSON (MpfCliInsertBatch pairs) = do
    let toKeyValue (MpfCliKey k, MpfCliValue v) =
          object ["key" .= encodeHexStr k, "value" .= encodeHexStr v]
    toJSON $ map toKeyValue pairs

instance FromJSON MpfCliInsertBatch where
  parseJSON = withObject "MpfCliInsertBatch" $ \v -> do
    pairs <- v .: "nodes"
    let parseKeyValue obj = do
          key <- obj .: "key"
          value <- obj .: "value"
          return (key, value)
    MpfCliInsertBatch <$> mapM parseKeyValue pairs

_insertBatch
  :: MpfCliPath
  -> MpfCliDbPath
  -> MpfCliInsertBatch
  -> IO (Either String (MpfCliResponse [InsertNodeResponse]), NominalDiffTime)
_insertBatch cliPath dbPath batch = do
  let args = ["insert-batch", "--nodes", L8.unpack (A.encode (toJSON batch))]
  runMpfCliJson cliPath dbPath args

_existsNode
  :: MpfCliPath
  -> MpfCliDbPath
  -> MpfCliKey
  -> IO (Either String (MpfCliResponse String))
_existsNode cliPath dbPath (MpfCliKey key) =
  fst <$> runMpfCliJson cliPath dbPath ["exists", "--key", encodeHexStr key]

_batchExists
  :: MpfCliPath
  -> MpfCliDbPath
  -> [MpfCliKey]
  -> IO (Either String (MpfCliResponse [String]))
_batchExists cliPath dbPath keys =
  let hexKeys = map (encodeHexStr . unMpfCliKey) keys
   in fst
        <$> runMpfCliJson
          cliPath
          dbPath
          ["batch-exists", "--keys", L8.unpack (A.encode (toJSON hexKeys))]

_getRoot
  :: MpfCliPath
  -> MpfCliDbPath
  -> IO (Either String (MpfCliResponse MpfRootHash))
_getRoot cliPath dbPath =
  fst <$> runMpfCliJson cliPath dbPath ["get-root-hash"]

_mkMembershipProof
  :: MpfCliPath
  -> MpfCliDbPath
  -> MpfCliKey
  -> IO (Either String (MpfCliResponse ProofResponse))
_mkMembershipProof cliPath dbPath (MpfCliKey key) =
  fst <$> runMpfCliJson cliPath dbPath ["verify", "--key", encodeHexStr key]

data MpfClient = MpfClient
  { insert
      :: MpfCliKey
      -> MpfCliValue
      -> IO (Either String (MpfCliResponse InsertNodeResponse))
  , insertBatch
      :: MpfCliInsertBatch
      -> IO (Either String (MpfCliResponse [InsertNodeResponse]))
  , insertBatchWithElapsed
      :: MpfCliInsertBatch
      -> IO (Either String (MpfCliResponse [InsertNodeResponse]), NominalDiffTime)
  , exists :: MpfCliKey -> IO (Either String (MpfCliResponse String))
  , batchExists
      :: [MpfCliKey]
      -> IO (Either String (MpfCliResponse [String]))
  , getRoot
      :: IO (Either String (MpfCliResponse MpfRootHash))
  , mkMembershipProof
      :: MpfCliKey
      -> IO (Either String (MpfCliResponse ProofResponse))
  }

mkMpfClient :: MpfCliPath -> MpfCliDbPath -> MpfClient
mkMpfClient cliPath dbPath =
  MpfClient
    { insert = _insertNode cliPath dbPath
    , insertBatch = fmap fst <$> _insertBatch cliPath dbPath
    , insertBatchWithElapsed = _insertBatch cliPath dbPath
    , exists = _existsNode cliPath dbPath
    , getRoot = _getRoot cliPath dbPath
    , mkMembershipProof = _mkMembershipProof cliPath dbPath
    , batchExists = _batchExists cliPath dbPath
    }

-- | Helper function to run mpf-cli and parse JSON output
runMpfCliJson
  :: (FromJSON a)
  => MpfCliPath
  -> MpfCliDbPath
  -> [String]
  -> IO (Either String (MpfCliResponse a), NominalDiffTime)
runMpfCliJson (MpfCliPath cliPath) (MpfCliDbPath dbPath) args = do
  let fullArgs = args <> ["--store-path", dbPath]
  _ <- evaluate fullArgs
  start <- getCurrentTime
  result <- try $ readProcess cliPath fullArgs ""
  end <- getCurrentTime
  let elapsed = diffUTCTime end start
  pure
    ( case result of
        Left (e :: IOException) ->
          Left $ "Command execution failed: " <> show e
        Right output ->
          case eitherDecode (L8.pack output) of
            Left err -> Left $ "Invalid JSON response: " <> err <> " in output " <> show output
            Right parsed -> Right parsed
    , elapsed
    )
