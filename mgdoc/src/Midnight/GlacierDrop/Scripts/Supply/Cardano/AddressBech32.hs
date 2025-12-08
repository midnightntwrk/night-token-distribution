{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Midnight.GlacierDrop.Scripts.Supply.Cardano.AddressBech32 (
  Network (..),
  HeaderType (..),
  AddressBech32 (rawBytes, network, headerType),
  parse,
  parseHeader,
  print,
  getCredentialsBytes,
  getPaymentCredentialBytes,
  getStakingCredentialBytes,
  toAddress,
  unsafeAddressBech32,
  addressBytes,
) where

import qualified Data.ByteString as BS
import Midnight.GlacierDrop.Scripts.Supply.Bech32 (Bech32 (..))
import qualified Midnight.GlacierDrop.Scripts.Supply.Bech32 as Bech32
import Midnight.GlacierDrop.Scripts.Supply.Byte (
  Byte,
  NibbleValue (NibbleValue),
  byteToByteValue,
  byteValueToNibbles,
  unconsByteString,
 )
import PlutusLedgerApi.V3 (
  Address (..),
  Credential (..),
  PubKeyHash (..),
  ScriptHash (..),
  StakingCredential (..),
 )
import PlutusPrelude (Generic)
import PlutusTx (makeLift)
import PlutusTx.Prelude
import PlutusTx.Show (deriveShow)
import Test.QuickCheck (
  Arbitrary (arbitrary),
  CoArbitrary,
  elements,
  genericCoarbitrary,
 )
import Test.QuickCheck.Arbitrary (CoArbitrary (coarbitrary))
import qualified Prelude as Haskell

-- | This is customized encoding of protected body:

-- Header type (t t t t . . . .)
--              Payment Part    Delegation Part     Total Length
-- (0) 0000.... PaymentKeyHash  StakeKeyHash        56
-- (1) 0001.... ScriptHash      StakeKeyHash        56
-- (2) 0010.... PaymentKeyHash  ScriptHash          56
-- (3) 0011.... ScriptHash      ScriptHash          56
-- (4) 0100.... PaymentKeyHash  Pointer             28< & <56
-- (5) 0101.... ScriptHash      Pointer             28< & <56
-- (6) 0110.... PaymentKeyHash  ø                   28
-- (7) 0111.... ScriptHash      ø                   28
--
-- Network Tag (. . . . n n n n)  Semantic
-- ....0000 Testnet(s)
-- ....0001 Mainnet

data Network = Mainnet | Testnet
  deriving (Haskell.Show, Haskell.Eq, Haskell.Ord, Generic)

instance Arbitrary Network where
  arbitrary = elements [Mainnet, Testnet]

instance CoArbitrary Network where
  coarbitrary = genericCoarbitrary

-- Value between 0 and 7 which encodes the address type
newtype HeaderType = HeaderType Integer
  deriving (Haskell.Show, Haskell.Eq, Haskell.Ord, Generic)

instance Arbitrary HeaderType where
  arbitrary = HeaderType Haskell.<$> elements [0, 1, 2, 3, 4, 5, 6, 7]

instance CoArbitrary HeaderType where
  coarbitrary = genericCoarbitrary

-- | Invariants:
-- | * header was correctly parsed
-- | * credentials bytes have **appropriate length** for the address type. By appropriate we mean only the
-- |   subset of cases which we support: 28 and 56 bytes - we don't support pointers.
-- | We can not derive valid checksums easily so the generator is implemented in the testing module.
data AddressBech32 = AddressBech32
  { headerType :: HeaderType
  , network :: Network
  , rawBytes :: BuiltinByteString
  -- ^ The first byte is a header byte
  -- | The 1-29 or 1~57 bytes (staking pointers are shorter) are the actual address bytes
  -- | The last 5 bytes are the bech32 checksum
  }
  deriving (Haskell.Show, Haskell.Eq, Haskell.Ord, Generic)

{-# NOINLINE addressBytes #-}
addressBytes :: AddressBech32 -> BS.ByteString
addressBytes =
  BS.dropEnd (Haskell.fromInteger checkSumLengthInBytes)
    . fromBuiltin
    . rawBytes

{-# INLINEABLE unsafeAddressBech32 #-}
unsafeAddressBech32
  :: HeaderType -> Network -> BuiltinByteString -> AddressBech32
unsafeAddressBech32 = AddressBech32

{-# INLINEABLE parseHeader #-}
parseHeader :: Byte -> Maybe (Network, HeaderType)
parseHeader byte = do
  let (NibbleValue addrNibbleInt, NibbleValue networkNibbleValue) = byteValueToNibbles . byteToByteValue $ byte
  network <-
    if
      | networkNibbleValue == 0 -> Just Testnet
      | networkNibbleValue == 1 -> Just Mainnet
      | otherwise -> Nothing
  address <-
    if addrNibbleInt < 8
      then Just $ HeaderType addrNibbleInt
      else Nothing
  pure (network, address)

--- In our two cases:
--  * For 57 bytes:
--    * 57 * 8 = 456 bits
--    * Bech32 adds 4 bits of padding
--    * 456 + 4 bits of padding + 30 bits checksum = 490 bits
--    * 6 bits of Word8 padding
--    * 496 bits total = 62 bytes
--    * 62 bytes - 57 bytes = 5 bytes of checksum
--  * For 29 bytes:
--    * 29 * 8 = 232 bits
--    * Bech32 adds 3 bits of padding
--    * 232 + 3 bits of padding + 30 bits checksum = 265 bits
--    * 7 bits of Word8 padding
--    * 272 bits total = 34 bytes
--    * 34 bytes - 29 bytes = 5 bytes of checksum
checkSumLengthInBytes :: Integer
checkSumLengthInBytes = 5

-- As explained above in both our cases we have more
-- than 4 bits of final padding so we should ignore
-- the last code point.
ignoredCheckSumCodePoint :: Integer
ignoredCheckSumCodePoint = 1

fullAddressLength :: Integer
fullAddressLength = 56

enterpriseAddressLength :: Integer
enterpriseAddressLength = 28

credentialLength :: Integer
credentialLength = 28

-- Basic address validation:

-- * check if the total length is correct

-- * check if header byte roughly makes sense
{-# INLINEABLE parse #-}
parse :: BuiltinByteString -> Maybe AddressBech32
parse bytes = do
  (headerByte, _) <- unconsByteString bytes
  (network, headerType@(HeaderType headerTypeValue)) <- parseHeader headerByte
  let credentialsBytes =
        sliceByteString 1 (lengthOfByteString bytes - (checkSumLengthInBytes + 1)) bytes
      credentialsLength = lengthOfByteString credentialsBytes
      expectedLength
        | headerTypeValue < 4 = fullAddressLength
        | headerTypeValue >= 6 = enterpriseAddressLength
        | otherwise = -1 -- pointer addresses not supported
  if expectedLength == credentialsLength
    then Just $ AddressBech32 headerType network bytes
    else Nothing

bitsPerByte :: Integer
bitsPerByte = 8

bitsPerBech32CodePoint :: Integer
bitsPerBech32CodePoint = 5

{-# INLINEABLE print #-}
print :: AddressBech32 -> BuiltinByteString
print (AddressBech32{network, rawBytes}) = do
  let -- We subtract one from the total number of code points because as explained above
      expectedLength = do
        let totalBits = lengthOfByteString rawBytes * bitsPerByte
            totalCodePoints = totalBits `quotient` bitsPerBech32CodePoint
        totalCodePoints - ignoredCheckSumCodePoint
      Bech32 allBech32Bytes = Bech32.encode rawBytes
      addrTail = sliceByteString 0 expectedLength allBech32Bytes
      addrHeader = case network of
        Mainnet -> "addr1"
        Testnet -> "addr_test1"
  addrHeader `appendByteString` addrTail

{-# INLINEABLE getCredentialsBytes #-}
getCredentialsBytes :: AddressBech32 -> BuiltinByteString
getCredentialsBytes (AddressBech32{rawBytes}) =
  -- Exclude header byte and last 5 bytes of bech32 checksum
  sliceByteString
    1
    (lengthOfByteString rawBytes - (checkSumLengthInBytes + 1))
    rawBytes

{-# INLINEABLE getPaymentCredentialBytes #-}
getPaymentCredentialBytes :: AddressBech32 -> BuiltinByteString
getPaymentCredentialBytes = sliceByteString 0 credentialLength . getCredentialsBytes

{-# INLINEABLE getStakingCredentialBytes #-}
getStakingCredentialBytes :: AddressBech32 -> Maybe BuiltinByteString
getStakingCredentialBytes addr = do
  let credentialsBytes = getCredentialsBytes addr
  if lengthOfByteString credentialsBytes == credentialLength
    then Nothing
    else
      Just
        $ sliceByteString
          credentialLength
          (lengthOfByteString credentialsBytes - credentialLength)
          credentialsBytes

{-# INLINEABLE toAddress #-}
toAddress :: AddressBech32 -> Address
toAddress AddressBech32{headerType = HeaderType headerType, rawBytes}
  | headerType == 0 =
      Address
        (PubKeyCredential $ PubKeyHash $ getCredential 0)
        ( Just
            $ StakingHash
            $ PubKeyCredential
            $ PubKeyHash
            $ getCredential credentialLength
        )
  | headerType == 1 =
      Address
        (ScriptCredential $ ScriptHash $ getCredential 0)
        ( Just
            $ StakingHash
            $ PubKeyCredential
            $ PubKeyHash
            $ getCredential credentialLength
        )
  | headerType == 2 =
      Address
        (PubKeyCredential $ PubKeyHash $ getCredential 0)
        ( Just
            $ StakingHash
            $ ScriptCredential
            $ ScriptHash
            $ getCredential credentialLength
        )
  | headerType == 3 =
      Address
        (ScriptCredential $ ScriptHash $ getCredential 0)
        ( Just
            $ StakingHash
            $ ScriptCredential
            $ ScriptHash
            $ getCredential credentialLength
        )
  | headerType == 6 =
      Address
        (PubKeyCredential $ PubKeyHash $ getCredential 0)
        Nothing
  | headerType == 7 =
      Address
        (ScriptCredential $ ScriptHash $ getCredential 0)
        Nothing
  | otherwise = traceError "Pointer addresses not supported"
  where
    -- skip the header
    getCredential offset = sliceByteString (offset + 1) credentialLength rawBytes

deriveShow ''Network
deriveShow ''HeaderType
deriveShow ''AddressBech32

makeLift ''Network
makeLift ''HeaderType
makeLift ''AddressBech32
