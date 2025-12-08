{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Midnight.GlacierDrop.Scripts.Supply.CBOR where

import GHC.ByteOrder (ByteOrder (BigEndian))
import PlutusTx.Prelude

newtype CBOR = CBOR {unCBOR :: BuiltinByteString}

-- Function fails for ByteStrings longer than 65535 bytes.
{-# INLINEABLE encodeSmallByteString #-}
encodeSmallByteString :: BuiltinByteString -> Maybe CBOR
encodeSmallByteString bs = do
  let attachHeader h = CBOR $ h `appendByteString` bs
  case lengthOfByteString bs of
    -- 0x4l
    l | l < 24 -> Just $ attachHeader $ integerToByteString BigEndian 0 (64 + l)
    -- 0x58 0xll
    l | l < 256 -> Just $ attachHeader $ integerToByteString BigEndian 0 (22528 + l)
    -- 0x59 0xll 0xll
    l
      | l < 65536 ->
          Just $ attachHeader $ integerToByteString BigEndian 0 (5832704 + l)
    _ -> Nothing
