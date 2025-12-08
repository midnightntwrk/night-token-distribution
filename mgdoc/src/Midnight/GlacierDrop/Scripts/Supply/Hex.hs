{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Midnight.GlacierDrop.Scripts.Supply.Hex where

import Language.Haskell.TH.Quote (QuasiQuoter (..))

import Data.ByteString qualified as BS
import Data.Char (chr)
import GHC.ByteOrder (ByteOrder (BigEndian))
import Language.Haskell.TH (
  Exp (..),
  Lit (StringL),
  Type (ConT),
  litE,
  lookupTypeName,
 )
import Language.Haskell.TH.Syntax (Q)
import Midnight.GlacierDrop.Scripts.Supply.Byte (
  Byte (Byte),
  ByteValue (ByteValue),
  NibbleValue (NibbleValue),
  byteToByteValue,
  byteValueToByte,
  byteValueToNibbles,
  unconsByteString,
 )
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import PlutusTx.Prelude as P
import Prelude qualified as H

-- | TODO: Most of the constant `BuiltinByteString` values
-- | should be created using this conversion function through
-- | the quasi quoter provided below to guarantee off-chain
-- | preprocessing.
-- | We encountered `BuiltinByteString` `IsString` instance
-- | limitations which prevents us from creating arbitrary bytes:
-- | https://github.com/paluh/plutus-experiments/blob/main/plutus-experiments/test/Spec.hs#L16
-- | For now we use a workaround function to do the preprocessing.
fromHexString :: H.String -> P.BuiltinByteString
fromHexString = \case
  (firstNibble : secondNibble : rest) -> do
    let firstNibble' = nibbleToHex firstNibble
    let secondNibble' = nibbleToHex secondNibble
    let byte = firstNibble' * 16 + secondNibble'
    P.integerToByteString BigEndian 1 byte <> fromHexString rest
  _ -> ""
  where
    nibbleToHex :: H.Char -> Integer
    nibbleToHex nibble = case nibble of
      '0' -> 0
      '1' -> 1
      '2' -> 2
      '3' -> 3
      '4' -> 4
      '5' -> 5
      '6' -> 6
      '7' -> 7
      '8' -> 8
      '9' -> 9
      'a' -> 10
      'b' -> 11
      'c' -> 12
      'd' -> 13
      'e' -> 14
      'f' -> 15
      _ -> H.error $ "Invalid hex code point:" <> [nibble]

-- Constructs a bytestring literal together with annotation:
-- ```
-- "\xff\x08" :: BuiltinByteString
-- ```
mkStaticBuiltinByteStringExp :: H.String -> Q Exp
mkStaticBuiltinByteStringExp hexStr = do
  let BuiltinByteString byteString = fromHexString hexStr
  let byteStringStr = H.map (chr . H.fromIntegral) $ BS.unpack byteString
  litStr <- litE (StringL byteStringStr)
  mName <- lookupTypeName "BuiltinByteString"
  case mName of
    Just name -> do
      let typeExp = ConT name
      let sigExp = SigE litStr typeExp
      return sigExp
    Nothing ->
      H.fail
        "To use the _0x quoter, you must import PlutusTx.Prelude (BuiltinByteString)"

-- |  This function is a workaround to the problem described above.
{-# INLINEABLE fromHex #-}
fromHex :: BuiltinByteString -> BuiltinByteString
fromHex bs
  | bs P.== "" = ""
  | otherwise = do
      let possibleBytes = do
            (firstNibble, bs') <- unconsByteString bs
            (secondNibble, bs'') <- unconsByteString bs'
            let NibbleValue firstNibbleValue = nibbleHexByteToInteger firstNibble
                NibbleValue secondNibbleValue = nibbleHexByteToInteger secondNibble
                byteValue = firstNibbleValue * 16 + secondNibbleValue
                Byte byte = byteValueToByte $ ByteValue byteValue
            pure $ byte `appendByteString` fromHex bs''
      case possibleBytes of
        Just bytes -> bytes
        Nothing -> P.error ()
  where
    nibbleHexByteToInteger :: Byte -> NibbleValue
    nibbleHexByteToInteger (Byte x)
      | x P.== "0" = NibbleValue 0
      | x P.== "1" = NibbleValue 1
      | x P.== "2" = NibbleValue 2
      | x P.== "3" = NibbleValue 3
      | x P.== "4" = NibbleValue 4
      | x P.== "5" = NibbleValue 5
      | x P.== "6" = NibbleValue 6
      | x P.== "7" = NibbleValue 7
      | x P.== "8" = NibbleValue 8
      | x P.== "9" = NibbleValue 9
      | x P.== "a" = NibbleValue 10
      | x P.== "b" = NibbleValue 11
      | x P.== "c" = NibbleValue 12
      | x P.== "d" = NibbleValue 13
      | x P.== "e" = NibbleValue 14
      | x P.== "f" = NibbleValue 15
      | otherwise = P.error ()

-- Build a call expression - for "ff8a34" it would look like:
-- ```
-- fromHex "ff8a34"
-- ```
mkFromHexCallExp :: H.String -> Q Exp
mkFromHexCallExp hex = do
  litExp <- litE (StringL hex)
  return $ AppE (VarE 'fromHex) litExp

_0x :: QuasiQuoter
_0x =
  QuasiQuoter
    { quoteExp = mkFromHexCallExp
    , quotePat = H.error "_0xQuoter cannot be used in pattern context"
    , quoteType = H.error "_0xQuoter cannot be used in type context"
    , quoteDec = H.error "_0xQuoter cannot be used in declaration context"
    }

{- HLINT ignore "Use guards" -}
{-# INLINEABLE nibbleValueToHex #-}
nibbleValueToHex :: NibbleValue -> BuiltinByteString
nibbleValueToHex (NibbleValue i) =
  if i > 7
    then -- 8, 9, 10, 11, 12, 13, 14, 15

      if i > 11
        then -- 12, 13, 14, 15

          if i > 13
            then -- 14, 15

              if i > 14
                then "f" -- 15
                else "e" -- 14
            else -- 12, 13

              if i > 12
                then "d" -- 13
                else "c" -- 12
        else
          if i > 9
            then -- 10, 11

              if i > 10
                then "b" -- 11
                else "a" -- 10
            else -- 8, 9

              if i > 8
                then "9"
                else "8"
    else -- 0, 1, 2, 3, 4, 5, 6, 7

      if i > 3
        then -- 4, 5, 6, 7

          if i > 5
            then -- 6, 7

              if i > 6
                then "7"
                else "6"
            else -- 4, 5

              if i > 4
                then "5"
                else "4"
        else -- 0, 1, 2, 3

          if i > 1
            then -- 2, 3

              if i > 2
                then "3"
                else "2"
            else -- 0, 1

              if i > 0
                then "1"
                else "0"

{-# INLINEABLE toHex #-}

-- | A useful helper for debugging purposes
toHex :: BuiltinByteString -> BuiltinByteString
toHex bs = case unconsByteString bs of
  Nothing -> ""
  Just (byte, bs') -> do
    let (firstNibbleValue, secondNibbleValue) = byteValueToNibbles . byteToByteValue $ byte
        firstNibbleHex = nibbleValueToHex firstNibbleValue
        secondNibbleHex = nibbleValueToHex secondNibbleValue
    firstNibbleHex `appendByteString` secondNibbleHex `appendByteString` toHex bs'
