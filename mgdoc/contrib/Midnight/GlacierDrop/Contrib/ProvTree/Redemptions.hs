module Midnight.GlacierDrop.Contrib.ProvTree.Redemptions (
  CheckRes (..),
  PathItem (..),
  ProofdRedemptionsClient (..),
  ProofReq,
  ProofRes (..),
  callProofIO,
  callProofM,
  callCheckIO,
  callCheckM,
  mkProofReq,
) where

import Cardano.Api as C
import Control.Error (note)
import Control.Exception (try)
import Control.Exception.Base (SomeException)
import Control.Lens (Getter, view)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (withObject, (.:), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import Data.Aeson.Types (FromJSON (..), object)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Traversable (for)
import GHC.Generics (Generic)
import Midnight.GlacierDrop.Contrib.Data.Aeson (HexEncoded (..))
import Network.HTTP.Simple (
  Response,
  defaultRequest,
  getResponseBody,
  getResponseStatus,
  getResponseStatusCode,
  httpLBS,
  setRequestBodyJSON,
  setRequestHost,
  setRequestMethod,
  setRequestPath,
  setRequestPort,
  setRequestSecure,
 )
import Servant.Client (BaseUrl (..), Scheme (..))
import Prelude hiding (minBound)

newtype ProofReq = ProofReq A.Value
  deriving (Show, Eq, Generic)
  deriving newtype (ToJSON)

mkProofReq
  :: C.Address C.ShelleyAddr
  -> ProofReq
mkProofReq destAddress =
  ProofReq $
    object
      [ "chain" .= ("cardano" :: Text)
      , "address" .= destAddress
      , "dest_address" .= destAddress
      ]

data ProofRes = ProofRes
  { root :: ByteString
  , globalIndex :: Integer
  , leafIndex :: Integer
  , path :: [PathItem]
  }
  deriving (Show, Eq, Generic)

data PathItem = PathItem
  { h :: ByteString
  , v :: Integer
  }
  deriving (Show, Eq, Generic)

instance FromJSON PathItem where
  parseJSON = withObject "PathItem" $ \v -> do
    HexEncoded h <- v .: "h"
    val <- v .: "v"
    pure $ PathItem h val

instance FromJSON ProofRes where
  parseJSON = withObject "ProofRes" $ \v -> do
    HexEncoded root <- v .: "root"
    leafIndex <- v .: "index"
    globalIndex <- v .: "globalIndex"
    mPath <- v .: "path"
    pathRes <- for mPath \val -> do
      case val of
        A.Object _ -> Just <$> parseJSON val
        A.String _ -> Just <$> parseJSON (A.Object (A.singleton "" (A.Number 0)))
        _ -> pure Nothing
    let path = catMaybes pathRes
    pure $ ProofRes{..}

data CheckRes = CheckRes
  { value :: Integer
  , jitterStratum :: Integer
  }
  deriving (Show, Eq, Generic)

instance FromJSON CheckRes where
  parseJSON = withObject "CheckRes" $ \v -> do
    value <- v .: "value"
    jitterStratum <- v .: "jitterStratum"
    pure $ CheckRes{..}

{-# NOINLINE safeDecodeUtf8 #-}
safeDecodeUtf8 :: ByteString -> Either Text Text
safeDecodeUtf8 bs = do
  case T.decodeUtf8' bs of
    Left _ -> Left (T.decodeUtf8 . Base16.encode $ bs)
    Right text -> Right text

callProofIO :: BaseUrl -> ProofReq -> IO (Either Text ProofRes)
callProofIO baseUrl proofReq = runExceptT do
  let BaseUrl scheme host port _ = baseUrl
      request =
        setRequestMethod "POST"
          . setRequestBodyJSON proofReq
          . setRequestHost (T.encodeUtf8 . T.pack $ host)
          . setRequestSecure (scheme == Https)
          . setRequestPort port
          . setRequestPath (T.encodeUtf8 "/proof")
          $ defaultRequest
  (possibleResponse :: Either SomeException (Response LBS.ByteString)) <-
    liftIO (try $ httpLBS request)
  response <- liftEither do
    first (T.pack . show) possibleResponse

  textResponse <- liftEither do
    let responseBody = getResponseBody response
    safeDecodeUtf8 $ LBS.toStrict responseBody

  liftEither $ do
    let showT :: (Show a) => a -> Text
        showT = T.pack . show
        responseStatus = getResponseStatus response
        responseStatusCode = getResponseStatusCode response

    note
      ( "Status code: "
          <> showT responseStatusCode
          <> "; Status: "
          <> showT responseStatus
          <> "; Invalid JSON: "
          <> textResponse
      )
      $ A.decodeStrictText textResponse

callCheckIO :: BaseUrl -> C.Address C.ShelleyAddr -> IO (Either Text CheckRes)
callCheckIO baseUrl addr = runExceptT do
  let addr' = C.serialiseToBech32 addr
      BaseUrl scheme host port _ = baseUrl
      request =
        setRequestMethod "GET"
          . setRequestHost (T.encodeUtf8 . T.pack $ host)
          . setRequestSecure (scheme == Https)
          . setRequestPort port
          . setRequestPath (T.encodeUtf8 $ "/check/cardano/" <> addr')
          $ defaultRequest
  (possibleResponse :: Either SomeException (Response LBS.ByteString)) <-
    liftIO (try $ httpLBS request)
  liftEither do
    response <- first (T.pack . show) possibleResponse
    let showT :: (Show a) => a -> Text
        showT = T.pack . show
        responseStatus = getResponseStatus response
        responseStatusCode = getResponseStatusCode response
        responseBody = getResponseBody response
    text <- safeDecodeUtf8 $ LBS.toStrict responseBody
    note
      ( "Status code: "
          <> showT responseStatusCode
          <> "; Status: "
          <> showT responseStatus
          <> "; Invalid JSON: "
          <> text
      )
      $ A.decodeStrictText text

-- A handy? monad stack - not sure if it should be here
newtype ProofdRedemptionsClient = ProofdRedemptionsClient BaseUrl

class WithProofdRedemptionsClientEnv env where
  proofdRedemptionsClientEnvL :: Getter env ProofdRedemptionsClient

type ProofdRedemptionsMonad env m =
  ( MonadReader env m
  , WithProofdRedemptionsClientEnv env
  , MonadIO m
  , MonadError Text m
  )

callProofM :: (ProofdRedemptionsMonad env m) => ProofReq -> m ProofRes
callProofM proofReq = do
  ProofdRedemptionsClient baseUrl <- view proofdRedemptionsClientEnvL
  liftEither =<< liftIO (callProofIO baseUrl proofReq)

callCheckM
  :: (ProofdRedemptionsMonad env m) => C.Address C.ShelleyAddr -> m CheckRes
callCheckM addr = do
  ProofdRedemptionsClient baseUrl <- view proofdRedemptionsClientEnvL
  liftEither =<< liftIO (callCheckIO baseUrl addr)
