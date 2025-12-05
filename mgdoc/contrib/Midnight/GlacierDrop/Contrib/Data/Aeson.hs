module Midnight.GlacierDrop.Contrib.Data.Aeson where

import Control.Monad ((<=<))
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  withText,
 )
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import qualified Data.ByteString.Base16 as Base16

encodeHex :: ByteString -> Text
encodeHex = decodeUtf8 . Base16.encode

decodeHex :: Text -> Parser ByteString
decodeHex t =
  case Base16.decode (encodeUtf8 t) of
    Right decoded -> pure decoded
    Left _ -> fail "Invalid hexadecimal encoding"

newtype HexEncoded = HexEncoded {getHexEncoded :: ByteString}
  deriving (Show, Eq, Ord)

instance ToJSON HexEncoded where
  toJSON (HexEncoded bs) = toJSON $ encodeHex bs

instance FromJSON HexEncoded where
  parseJSON = withText "HexEncoded" (pure . HexEncoded <=< decodeHex)
