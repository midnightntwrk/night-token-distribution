{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Midnight.GlacierDrop.Scripts.Supply.Byte where

import GHC.ByteOrder (ByteOrder (BigEndian))
import PlutusTx.Prelude
import qualified Prelude as Haskell

newtype ByteValue = ByteValue Integer
  deriving newtype (Eq, Ord, Haskell.Eq, Haskell.Show)

{-# INLINEABLE byteToByteValue #-}
byteToByteValue :: Byte -> ByteValue
byteToByteValue (Byte b) = ByteValue $ byteStringToInteger BigEndian b

{-# INLINEABLE byteValueToByte #-}
byteValueToByte :: ByteValue -> Byte
byteValueToByte (ByteValue i) = Byte $ integerToByteString BigEndian 1 i

{-# INLINEABLE getByteValue #-}
getByteValue :: ByteValue -> Integer
getByteValue (ByteValue i) = i

instance Semigroup ByteValue where
  ByteValue a <> ByteValue b = ByteValue $ (a + b) `modulo` 256

instance Monoid ByteValue where
  mempty = ByteValue 0

newtype Byte = Byte BuiltinByteString

{-# INLINEABLE shiftByteR #-}
shiftByteR :: ByteValue -> Integer -> ByteValue
shiftByteR (ByteValue b) n
  | n == 1 = ByteValue $ b `quotient` 2
  | n == 2 = ByteValue $ b `quotient` 4
  | n == 3 = ByteValue $ b `quotient` 8
  | n == 4 = ByteValue $ b `quotient` 16
  | n == 5 = ByteValue $ b `quotient` 32
  | n == 6 = ByteValue $ b `quotient` 64
  | otherwise = ByteValue $ b `quotient` 128

{-# INLINEABLE shiftByteL #-}
shiftByteL :: ByteValue -> Integer -> ByteValue
shiftByteL = do
  let go n v
        | n == 1 = v * 2
        | n == 2 = v * 4
        | n == 3 = v * 8
        | n == 4 = v * 16
        | n == 5 = v * 32
        | n == 6 = v * 64
        | otherwise = v * 128
  \(ByteValue b) n -> ByteValue $ go n b `modulo` 256

{- HLINT ignore "Use splitAt" -}
{-# INLINEABLE unconsByteString #-}
unconsByteString :: BuiltinByteString -> Maybe (Byte, BuiltinByteString)
unconsByteString bs | lengthOfByteString bs == 0 = Nothing
unconsByteString bs = Just (Byte $ takeByteString 1 bs, dropByteString 1 bs)

newtype NibbleValue = NibbleValue Integer
  deriving newtype (Eq, Ord, Haskell.Eq, Haskell.Show)

{-# INLINEABLE byteValueToNibbles #-}
byteValueToNibbles :: ByteValue -> (NibbleValue, NibbleValue)
byteValueToNibbles (ByteValue i) = do
  let firstNibble = i `quotient` 16
      secondNibble = i `modulo` 16
  (NibbleValue firstNibble, NibbleValue secondNibble)
