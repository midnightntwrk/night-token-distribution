{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Midnight.GlacierDrop.Scripts.Supply.Bech32 where

import Codec.Binary.Bech32.Internal qualified as Bech32
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char qualified as Char
import Data.Functor.Identity (Identity (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import Midnight.GlacierDrop.Scripts.Supply.Byte (
  ByteValue (ByteValue),
  byteToByteValue,
  shiftByteL,
  shiftByteR,
  unconsByteString,
 )
import PlutusTx.Foldable (fold)
import PlutusTx.List (length)
import PlutusTx.Prelude
import PlutusTx.Traversable (traverse)
import Prelude qualified as Haskell

-- 5 bit value: 0-31
newtype CodePoint = CodePoint Integer

{-# INLINEABLE codePointFromInteger #-}
codePointFromInteger :: Integer -> CodePoint
codePointFromInteger i = CodePoint $ i `modulo` 32

newtype Bech32 = Bech32 {unBech32 :: BuiltinByteString}
  deriving newtype
    ( Eq
    , Ord
    , Semigroup
    , Monoid
    , Haskell.Semigroup
    , Haskell.Monoid
    , Haskell.Eq
    , Haskell.Ord
    , Haskell.Show
    )

-- | We encode a point using maximally five comparisons.

{- HLINT ignore "Use guards" -}
{-# INLINEABLE encodeCodePoint #-}
encodeCodePoint :: CodePoint -> Bech32
encodeCodePoint (CodePoint i) =
  Bech32
    $ if i > 15
      then
        if i > 23 -- 16-31
          then
            if i > 27 -- 24-31
              then
                if i > 29 -- 28-31
                  then
                    if i > 30 -- 30-31
                      then "l" -- 31
                      else "7" -- 30
                  else
                    if i > 28 -- 28-29
                      then "a" -- 29
                      else "u" -- 28
              else
                if i > 25 -- 24-27
                  then
                    if i > 26 -- 26-27
                      then "m" -- 27
                      else "6" -- 26
                  else
                    if i > 24 -- 24-25
                      then "e" -- 25
                      else "c" -- 24
          else
            if i > 19 -- 16-23
              then
                if i > 21 -- 20-23
                  then
                    if i > 22 -- 22-23
                      then "h" -- 23
                      else "k" -- 22
                  else
                    if i > 20 -- 20-21
                      then "4" -- 21
                      else "5" -- 20
              else
                if i > 17 -- 16-19
                  then
                    if i > 18 -- 18-19
                      then "n" -- 19
                      else "j" -- 18
                  else
                    if i > 16 -- 16-17
                      then "3" -- 17
                      else "s" -- 16
      else
        if i > 7 -- 0-15
          then
            if i > 11 -- 8-15
              then
                if i > 13 -- 12-15
                  then
                    if i > 14 -- 14-15
                      then "0" -- 15
                      else "w" -- 14
                  else
                    if i > 12 -- 12-13
                      then "d" -- 13
                      else "v" -- 12
              else
                if i > 9 -- 8-11
                  then
                    if i > 10 -- 10-11
                      then "t" -- 11
                      else "2" -- 10
                  else
                    if i > 8 -- 8-9
                      then "f" -- 9
                      else "g" -- 8
          else
            if i > 3 -- 0-7
              then
                if i > 5 -- 4-7
                  then
                    if i > 6 -- 6-7
                      then "8" -- 7
                      else "x" -- 6
                  else
                    if i > 4 -- 4-5
                      then "9" -- 5
                      else "y" -- 4
              else
                if i > 1 -- 0-3
                  then
                    if i > 2 -- 2-3
                      then "r" -- 3
                      else "z" -- 2
                  else
                    if i > 0 -- 0-1
                      then "p" -- 1
                      else "q" -- 0

-- Already shifted bits of the bech32 byte prefix
type CodePointPrefix = ByteValue

-- Suffix bytes of the bech32 byte prefixed padded with zeros
type CodePointSuffix = ByteValue

{-# INLINEABLE byteValueToCodePoint #-}
byteValueToCodePoint :: ByteValue -> CodePoint
byteValueToCodePoint (ByteValue i) = codePointFromInteger i

{-# INLINEABLE encodeFiveThree #-}
encodeFiveThree :: BuiltinByteString -> [Bech32]
encodeFiveThree bs = case unconsByteString bs of
  Just (b, bs') -> do
    let i = byteToByteValue b
        bech32Char = do
          let codePoint = byteValueToCodePoint $ i `shiftByteR` 3
          encodeCodePoint codePoint
        nextCodePointPrefix = i `shiftByteL` 2
        bech32' = encodeTwoFiveOne nextCodePointPrefix bs'
    bech32Char : bech32'
  Nothing -> []

{-# INLINEABLE encodeTwoFiveOne #-}
encodeTwoFiveOne :: CodePointPrefix -> BuiltinByteString -> [Bech32]
encodeTwoFiveOne prefix bs = case unconsByteString bs of
  Just (b, bs') -> do
    let i = byteToByteValue b
        prevCodePointSuffix = i `shiftByteR` 6
        bech32Char = encodeCodePoint $ byteValueToCodePoint $ prefix <> prevCodePointSuffix
        bech32Char' = do
          let codePoint = byteValueToCodePoint $ i `shiftByteR` 1
          encodeCodePoint codePoint
        nextCodePointPrefix = i `shiftByteL` 4
        bech32' = encodeFourFour nextCodePointPrefix bs'
    bech32Char : bech32Char' : bech32'
  Nothing -> []

{-# INLINEABLE encodeFourFour #-}
encodeFourFour :: CodePointPrefix -> BuiltinByteString -> [Bech32]
encodeFourFour prefix bs = case unconsByteString bs of
  Just (b, bs') -> do
    let i = byteToByteValue b
        prevCodePointSuffix = i `shiftByteR` 4
        bech32Char = encodeCodePoint $ byteValueToCodePoint $ prefix <> prevCodePointSuffix
        nextCodePointPrefix = i `shiftByteL` 1
        bech32' = encodeOneFiveTwo nextCodePointPrefix bs'
    bech32Char : bech32'
  Nothing -> []

{-# INLINEABLE encodeOneFiveTwo #-}
encodeOneFiveTwo :: CodePointPrefix -> BuiltinByteString -> [Bech32]
encodeOneFiveTwo prefix bs = case unconsByteString bs of
  Just (b, bs') -> do
    let i = byteToByteValue b
        prevCodePointSuffix = i `shiftByteR` 7
        bech32Char = encodeCodePoint $ byteValueToCodePoint $ prefix <> prevCodePointSuffix
        bech32Char' = do
          let codePoint = byteValueToCodePoint $ i `shiftByteR` 2
          encodeCodePoint codePoint
        nextCodePointPrefix = i `shiftByteL` 3
        bech32' = encodeThreeFive nextCodePointPrefix bs'
    bech32Char : bech32Char' : bech32'
  Nothing -> []

{-# INLINEABLE encodeThreeFive #-}
encodeThreeFive :: CodePointPrefix -> BuiltinByteString -> [Bech32]
encodeThreeFive prefix bs = case unconsByteString bs of
  Just (b, bs') -> do
    let i = byteToByteValue b
        prevCodePointSuffix = i `shiftByteR` 5
        bech32Char = encodeCodePoint $ byteValueToCodePoint $ prefix <> prevCodePointSuffix
        bech32Char' = do
          let codePoint = byteValueToCodePoint i
          encodeCodePoint codePoint
        bech32' = encodeFiveThree bs'
    bech32Char : bech32Char' : bech32'
  Nothing -> []

{-# INLINEABLE encode #-}

-- | This function expectes full bytes:
-- | * It encodes every byte till exhaustion
-- | * So you can not use it to clearly specify some edge cases - for example:
-- |  - Let say that we have 10 bits to encode
-- |  - We have to pass 16 bits to the function
-- |  - We will get back 3 code points and we should strip the last one externally
-- | * It does not compute the checksum
encode :: BuiltinByteString -> Bech32
encode = fold . encodeFiveThree

-- The following decoding helpers are never to be used on-chain

-- Last byte can contain padding which should be sometimes ignored:

-- * Bech32 padds the data:

--    * say we have 2 bytes of data = 16 bits
--    * bech32 wants to create 5 bit code points
--    * so it pads these 16 bits to 20 bits (4 bits padding)

-- * bech32 adds 30 bits of checksum so we have 50 bits total

-- * then we are converting this back to Word8

-- * we have to add padding during that process so it fits into Word8

-- * so we build 56 bits of data (6 bits of new padding)

-- * we should ignore these 6 bits of padding when performing encoding
data Bech32BytesWithChecksum = Bech32BytesWithChecksum
  { prefix :: Text
  , bech32Bytes :: ByteString
  , lastCharPadding :: Bool
  }

-- Decodes a bech32 string, but preserves the checksum with padding.
decodeWithChecksum :: Text -> Either Haskell.String Bech32BytesWithChecksum
decodeWithChecksum text = do
  codePointsWithChecksum <-
    maybe (Left "Could not parse data with checksum part") Right
      . parseDataWithChecksumPart
      $ text
  hrp <-
    maybe (Left "Could not parse human readable part") Right
      $ getHumanReadablePart text
  let totalBits = length codePointsWithChecksum * 5
      paddingLength = do
        let rest = totalBits `Haskell.mod` 8
        if rest Haskell.== 0
          then 0
          else 8 Haskell.- rest
      bytes = BS.pack . toBase256WithPadding $ codePointsWithChecksum
  pure $ Bech32BytesWithChecksum hrp bytes (paddingLength Haskell.>= 5)

newtype Prefix = Prefix Text

mainnetPrefix :: Prefix
mainnetPrefix = Prefix "addr"

testnetPrefix :: Prefix
testnetPrefix = Prefix "addr_test"

attachBech32Checksum :: Prefix -> ByteString -> Bech32BytesWithChecksum
attachBech32Checksum (Prefix prefix) bytes = do
  let hp = case Bech32.humanReadablePartFromText prefix of
        Left e -> Haskell.error $ Haskell.show e
        Right p -> p
      dataPart = Bech32.dataPartFromBytes bytes
      encoded = Bech32.encodeLenient hp dataPart
  case decodeWithChecksum encoded of
    Left e -> Haskell.error e
    Right res -> res

parseDataWithChecksumPart :: Text -> Maybe [Bech32.Word5]
parseDataWithChecksumPart bech32 = do
  dcpUnparsed <- getDataPart $ T.map Char.toLower bech32
  traverse Bech32.dataCharToWord $ T.unpack dcpUnparsed

getDataPart :: Text -> Maybe Text
getDataPart s = do
  let s' = T.reverse s
  i <- T.findIndex (Haskell.== Bech32.separatorChar) s'
  pure $ T.reverse $ T.take i s'

getHumanReadablePart :: Text -> Maybe Text
getHumanReadablePart s = do
  let s' = T.reverse s
  i <- T.findIndex (Haskell.== Bech32.separatorChar) s'
  pure $ T.reverse $ T.drop (i Haskell.+ 1) s'

toBase256WithPadding :: [Bech32.Word5] -> [Word8]
toBase256WithPadding dat = do
  let toWord word = Haskell.fromIntegral (Bech32.getWord5 word)
  Haskell.map (Haskell.fromIntegral :: Haskell.Word -> Word8)
    $ runIdentity (Bech32.convertBits (Haskell.map toWord dat) 5 8 Bech32.yesPadding)
