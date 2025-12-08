{-# HLINT ignore "Use const" #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Midnight.GlacierDrop.Api where

import Control.Monad (replicateM)
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import GHC.ByteOrder (ByteOrder (BigEndian))
import GHC.Generics (Generic)
import Midnight.GlacierDrop.Scripts.Supply.Cardano.AddressBech32 (
  AddressBech32,
  unsafeAddressBech32,
 )
import Midnight.GlacierDrop.Scripts.Supply.Cardano.AddressBech32 qualified as AddressBech32
import PlutusLedgerApi.V1.Time (DiffMilliSeconds (..))
import PlutusLedgerApi.V3 hiding (
  ScriptContext (..),
  ScriptInfo (..),
  TxInfo (..),
 )
import PlutusTx
import PlutusTx.AssocMap qualified as AMap
import PlutusTx.Builtins (
  chooseData,
  mkConstr,
  mkI,
  unsafeDataAsConstr,
 )
import PlutusTx.Prelude qualified as P
import PlutusTx.Show (deriveShow)
import Test.QuickCheck (
  Arbitrary (..),
  CoArbitrary (..),
  Gen,
  Positive (..),
  chooseInt,
  elements,
  genericShrink,
  oneof,
 )
import Test.QuickCheck.Arbitrary (genericCoarbitrary)
import Test.QuickCheck.Instances ()
import Prelude

newtype Sha512HalfScriptHash = Sha512HalfScriptHash ScriptHash
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (ToData, FromData, UnsafeFromData)

newtype MerkleHash = MerkleHash
  { getMerkleHash :: BuiltinByteString
  }
  deriving stock (Eq, Ord, Generic)
  deriving newtype
    ( ToData
    , FromData
    , UnsafeFromData
    , P.Eq
    , P.Ord
    )
  deriving (Show) via LedgerBytes

-- | * We have to differentiate P2WPKH and P2PKH script
-- | addresses during BIP0322 verification.
-- | * We use `AnySecp256k1PublicKey` because
-- | Bitcoin legacy addresses hashing of
-- | compressed and uncompressed public keys.
data BitcoinScriptPubKeyInfo
  = P2TRPubKey SchnorrSecp256k1PublicKey
  | P2WPKHPubKey AnySecp256k1PublicKey
  | P2PKHPubKey AnySecp256k1PublicKey
  deriving stock (Show, Eq, Ord, Generic)

data SighashFlag
  = SIGHASH_ALL
  | SIGHASH_DEFAULT
  deriving stock (Show, Eq, Ord, Generic)

data BitcoinSigningScheme
  = BIP0137 AnySecp256k1PublicKey
  | -- | TapRoot can only be used against BIP0322 singing scheme
    BIP0322 BitcoinScriptPubKeyInfo SighashFlag
  deriving stock (Show, Eq, Ord, Generic)

newtype PlainMessage = PlainMessage {getPlainMessage :: BuiltinByteString}
  deriving newtype (Eq, Ord, Show)

-- We discovered that Solana toolchain (wallets, cli and specs) uses
-- at least three different formats of signatures.
data SolanaSignature
  = SolanaVerbatim
  | SolanaCLIFormat
  | SolanaOffchainMessage
  deriving stock (Show, Eq, Ord, Generic)

data CardanoSigningScheme
  = FakeTransactionSigningScheme
  | CIP8SigningScheme CIP8ProtectedHeaderInfo
  deriving stock (Show, Eq, Ord, Generic)

-- The protected header which is part of the signed payload can contain custom
-- data or only "standard" (not really a standard) pieces (kid=pkh, alg=-8).
-- If a given signature conforms to the above "standard" (all tested wallets do),
-- we can reconstruct this header ourselves.
data CIP8ProtectedHeaderInfo
  = NonStandardProtectedHeader BuiltinByteString
  | -- | Standard protected header contains all the address bytes.
    -- We could use `AddressBech32` type to represent it but
    -- we don't really care about it's validity here - we treat
    -- it as opaque part of the necessary wrapping of the CIP8
    -- envelope around message which should be signed.
    -- It is true that 32 bytes of the above address overlap with
    -- the pubKeyHash passed separately but reconstructing the full
    -- bytestring can actually consume more resources then just
    -- accepting it as a whole.
    StandardProtectedHeader BuiltinByteString
  deriving (Eq, Ord, Show, Generic)

data VerifiableClaimCredential
  = CardanoShelleyCredential
      CardanoSigningScheme
      Ed25519PublicKey
  | CardanoByronCredential
      AddressAttributes
      Ed25519PublicKey
      Bip32ChainCode
  | BitcoinCredential
      BitcoinSigningScheme
  | EthereumCredential
      Secp256k1PublicKey
  | BnbCredential
      Secp256k1PublicKey
  | BatCredential
      Secp256k1PublicKey
  | AvaxCredential
      Secp256k1PublicKey
  | BaseCredential
      Secp256k1PublicKey
  | SolanaCredential
      Ed25519PublicKey
      SolanaSignature
  | XrpCredential
      Ed25519orSecp256k1PublicKey
  deriving stock (Show, Eq, Ord, Generic)

newtype AddressAttributes = AddressAttributes
  { getAddressAttributes :: BuiltinByteString
  }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (ToData, FromData, UnsafeFromData, P.Eq, P.Ord)
  deriving (Show) via LedgerBytes

newtype Ed25519orSecp256k1PublicKey = Ed25519orSecp256k1PublicKey
  { getEd25519orSecp256k1PublicKey :: BuiltinByteString
  }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (ToData, FromData, UnsafeFromData, P.Eq, P.Ord)
  deriving (Show) via LedgerBytes

newtype Ed25519PublicKey = Ed25519PublicKey
  { getEd25519PublicKey :: BuiltinByteString
  }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (ToData, FromData, UnsafeFromData, P.Eq, P.Ord)
  deriving (Show) via LedgerBytes

-- 64 bytes uncompressed key
newtype Secp256k1PublicKey = Secp256k1PublicKey
  { getSecp256k1PublicKey :: BuiltinByteString
  }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (ToData, FromData, UnsafeFromData, P.Eq, P.Ord)
  deriving (Show) via LedgerBytes

{-# INLINEABLE compressSecp256k1PublicKey #-}
compressSecp256k1PublicKey
  :: Secp256k1PublicKey -> CompressedSecp256k1PublicKey
compressSecp256k1PublicKey (Secp256k1PublicKey pubKey) = do
  let pubKeyHeader =
        if (P.byteStringToInteger BigEndian (P.sliceByteString 63 1 pubKey) `P.modulo` 2)
          P.== 0
          then "\x02"
          else "\x03"
  CompressedSecp256k1PublicKey $
    pubKeyHeader
      `P.appendByteString` P.sliceByteString (0 :: Integer) (32 :: Integer) pubKey

-- 33 bytes compressed key
newtype CompressedSecp256k1PublicKey = CompressedSecp256k1PublicKey
  { getCompressedSecp256k1PublicKey :: BuiltinByteString
  }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (ToData, FromData, UnsafeFromData, P.Eq, P.Ord)
  deriving (Show) via LedgerBytes

data AnySecp256k1PublicKey
  = AnySecp256k1PublicKey Secp256k1PublicKey
  | AnyCompressedSecp256k1PublicKey CompressedSecp256k1PublicKey
  deriving stock (Show, Eq, Ord, Generic)

{-# INLINEABLE mkCompressedSecp256k1PublicKey #-}
mkCompressedSecp256k1PublicKey
  :: AnySecp256k1PublicKey -> CompressedSecp256k1PublicKey
mkCompressedSecp256k1PublicKey = \case
  (AnySecp256k1PublicKey uncompressed) -> compressSecp256k1PublicKey uncompressed
  (AnyCompressedSecp256k1PublicKey compressed) -> compressed

newtype SchnorrSecp256k1PublicKey = SchnorrSecp256k1PublicKey
  { getSchnorrSecp256k1PublicKey :: BuiltinByteString
  }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (ToData, FromData, UnsafeFromData, P.Eq, P.Ord)
  deriving (Show) via LedgerBytes

newtype Bip32ChainCode = Bip32ChainCode
  { getBip32ChainCode :: BuiltinByteString
  }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (ToData, FromData, UnsafeFromData, P.Eq, P.Ord)
  deriving (Show) via LedgerBytes

newtype Signature = Signature
  { getSignature :: BuiltinByteString
  }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (ToData, FromData, UnsafeFromData, P.Eq, P.Ord)
  deriving (Show) via LedgerBytes

newtype Star = Star
  { getStar :: Integer
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype
    ( ToData
    , FromData
    , UnsafeFromData
    , P.Eq
    , P.Ord
    , P.AdditiveSemigroup
    , P.AdditiveGroup
    , P.AdditiveMonoid
    , Num
    )
  deriving (Arbitrary) via (Positive Integer)

{-# INLINEABLE starValue #-}
starValue :: CurrencySymbol -> TokenName -> Star -> Value
starValue symbol name = singleton symbol name . getStar

deriving via (Positive Integer) instance (Arbitrary POSIXTime)
deriving via (Positive Integer) instance (Arbitrary DiffMilliSeconds)

-- | A variant of 'PlutusLedgerApi.V3.ScriptContext' that only includes the
-- data we need, uses domain types for the redeemer and datum, and moves
-- the redeemer into the 'ScriptInfo'
data ScriptContext mintRedeemer spendRedeemer datum = ScriptContext
  { scriptContextTxInfo :: TxInfo
  , scriptContextScriptInfo :: ScriptInfo mintRedeemer spendRedeemer datum
  }
  deriving stock (Eq, Show, Generic)

-- | A variant of 'PlutusLedgerApi.V3.ScriptInfo' that only decodes what our
-- scripts need, includes the redeemer, and uses a domain type for the redeemer and datum.
data ScriptInfo mintRedeemer spendRedeemer datum
  = MintingScript mintRedeemer CurrencySymbol
  | SpendingScript spendRedeemer TxOutRef (P.Maybe datum)
  deriving stock (Eq, Show, Generic)

-- | A version of PlutusLedgerApi.V3.TxInfo that only decodes what our
-- scripts need.
data TxInfo = TxInfo
  { txInfoInputs :: [TxInInfo]
  , txInfoReferenceInputs :: [TxInInfo]
  , txInfoOutputs :: [TxOut]
  , txInfoMint :: Value
  , txInfoValidRange :: POSIXTimeRange
  , txInfoSignatories :: [PubKeyHash]
  , txInfoRedeemers :: Map ScriptPurpose Redeemer
  }
  deriving stock (Show, Eq, Generic)

instance ToData TxInfo where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData TxInfo{..} =
    mkConstr
      0
      [ toBuiltinData txInfoInputs
      , toBuiltinData txInfoReferenceInputs
      , toBuiltinData txInfoOutputs
      , mkI 0
      , toBuiltinData txInfoMint
      , mkI 0
      , mkI 0
      , toBuiltinData txInfoValidRange
      , toBuiltinData txInfoSignatories
      , toBuiltinData txInfoRedeemers
      , mkI 0
      , mkI 0
      , mkI 0
      , mkI 0
      , mkI 0
      , mkI 0
      ]

instance UnsafeFromData TxInfo where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData d = case unsafeDataAsConstr d of
    ( _
      , (unsafeFromBuiltinData -> txInfoInputs)
          : (unsafeFromBuiltinData -> txInfoReferenceInputs)
          : (unsafeFromBuiltinData -> txInfoOutputs)
          : _
          : (unsafeFromBuiltinData -> txInfoMint)
          : _
          : _
          : (unsafeFromBuiltinData -> txInfoValidRange)
          : (unsafeFromBuiltinData -> txInfoSignatories)
          : (unsafeFromBuiltinData -> txInfoRedeemers)
          : _
      ) -> TxInfo{..}
    _ -> P.error ()

instance FromData TxInfo where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData d =
    chooseData
      d
      ( \_ -> case unsafeDataAsConstr d of
          ( _
            , inputs
                : referenceInputs
                : outputs
                : _
                : txInfoMint
                : _
                : _
                : validRange
                : signatories
                : txInfoRedeemers
                : _
            ) ->
              TxInfo
                P.<$> fromBuiltinData inputs
                P.<*> fromBuiltinData referenceInputs
                P.<*> fromBuiltinData outputs
                P.<*> fromBuiltinData txInfoMint
                P.<*> fromBuiltinData validRange
                P.<*> fromBuiltinData signatories
                P.<*> fromBuiltinData txInfoRedeemers
          _ -> P.Nothing
      )
      (\_ -> P.Nothing)
      (\_ -> P.Nothing)
      (\_ -> P.Nothing)
      (\_ -> P.Nothing)
      ()

instance
  (ToData mintRedeemer, ToData spendRedeemer, ToData datum)
  => ToData (ScriptContext mintRedeemer spendRedeemer datum)
  where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData ScriptContext{..} =
    mkConstr
      0
      [ toBuiltinData scriptContextTxInfo
      , redeemerData
      , infoData
      ]
    where
      (redeemerData, infoData) = case scriptContextScriptInfo of
        MintingScript redeemer currencySymbol -> (toBuiltinData redeemer, mkConstr 0 [toBuiltinData currencySymbol])
        SpendingScript redeemer ownRef datum ->
          ( toBuiltinData redeemer
          , mkConstr 1 [toBuiltinData ownRef, toBuiltinData datum]
          )

instance
  ( UnsafeFromData mintRedeemer
  , UnsafeFromData spendRedeemer
  , UnsafeFromData datum
  )
  => UnsafeFromData (ScriptContext mintRedeemer spendRedeemer datum)
  where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData d = case unsafeDataAsConstr d of
    ( _
      , [ unsafeFromBuiltinData -> scriptContextTxInfo
          , redeemerData
          , infoData
          ]
      ) -> ScriptContext{..}
        where
          scriptContextScriptInfo = case unsafeDataAsConstr infoData of
            ( tag
              , [unsafeFromBuiltinData -> currencySymbol]
              )
                | tag P.== 0 ->
                    MintingScript (unsafeFromBuiltinData redeemerData) currencySymbol
            ( tag
              , [ unsafeFromBuiltinData -> ownRef
                  , unsafeFromBuiltinData -> datum
                  ]
              )
                | tag P.== 1 -> SpendingScript (unsafeFromBuiltinData redeemerData) ownRef datum
            _ -> P.error ()
    _ -> P.error ()

instance
  (FromData mintRedeemer, FromData spendRedeemer, FromData datum)
  => FromData (ScriptContext mintRedeemer spendRedeemer datum)
  where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData d =
    chooseData
      d
      ( \_ -> case unsafeDataAsConstr d of
          ( _
            , [ fromBuiltinData -> P.Just scriptContextTxInfo
                , redeemerData
                , infoData
                ]
            ) -> ScriptContext scriptContextTxInfo P.<$> scriptContextScriptInfo
              where
                scriptContextScriptInfo =
                  chooseData
                    infoData
                    ( \_ -> case unsafeDataAsConstr infoData of
                        ( tag
                          , [fromBuiltinData -> P.Just currencySymbol]
                          )
                            | tag P.== 0 ->
                                flip MintingScript currencySymbol P.<$> fromBuiltinData redeemerData
                        ( tag
                          , [ fromBuiltinData -> P.Just ownRef
                              , fromBuiltinData -> P.Just datum
                              ]
                          )
                            | tag P.== 1 ->
                                (\redeemer -> SpendingScript redeemer ownRef datum)
                                  P.<$> fromBuiltinData redeemerData
                        _ -> P.Nothing
                    )
                    (\_ -> P.Nothing)
                    (\_ -> P.Nothing)
                    (\_ -> P.Nothing)
                    (\_ -> P.Nothing)
                    ()
          _ -> P.Nothing
      )
      (\_ -> P.Nothing)
      (\_ -> P.Nothing)
      (\_ -> P.Nothing)
      (\_ -> P.Nothing)
      ()

makeLift ''TxInfo
makeLift ''ScriptInfo
makeLift ''ScriptContext

deriveShow ''POSIXTime
deriveShow ''DiffMilliSeconds
deriveShow ''CurrencySymbol
deriveShow ''TokenName
deriveShow ''Star
deriveShow ''MerkleHash
deriveShow ''AddressAttributes
deriveShow ''Bip32ChainCode
deriveShow ''Secp256k1PublicKey
deriveShow ''CompressedSecp256k1PublicKey
deriveShow ''AnySecp256k1PublicKey
deriveShow ''Ed25519PublicKey
deriveShow ''Ed25519orSecp256k1PublicKey
deriveShow ''Signature
deriveShow ''SchnorrSecp256k1PublicKey
deriveShow ''BitcoinScriptPubKeyInfo
deriveShow ''SighashFlag
deriveShow ''BitcoinSigningScheme
deriveShow ''SolanaSignature
deriveShow ''CIP8ProtectedHeaderInfo
deriveShow ''CardanoSigningScheme
deriveShow ''VerifiableClaimCredential
deriveShow ''Sha512HalfScriptHash

makeLift ''Star
makeLift ''MerkleHash
makeLift ''AddressAttributes
makeLift ''Bip32ChainCode
makeLift ''Secp256k1PublicKey
makeLift ''CompressedSecp256k1PublicKey
makeLift ''AnySecp256k1PublicKey
makeLift ''Ed25519PublicKey
makeLift ''Ed25519orSecp256k1PublicKey
makeLift ''Signature
makeLift ''SchnorrSecp256k1PublicKey
makeLift ''BitcoinScriptPubKeyInfo
makeLift ''SighashFlag
makeLift ''BitcoinSigningScheme
makeLift ''SolanaSignature
makeLift ''CIP8ProtectedHeaderInfo
makeLift ''CardanoSigningScheme
makeLift ''VerifiableClaimCredential
makeLift ''Sha512HalfScriptHash

instance ToData AnySecp256k1PublicKey where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData = \case
    AnySecp256k1PublicKey pk -> toBuiltinData pk
    AnyCompressedSecp256k1PublicKey pk -> toBuiltinData pk

instance UnsafeFromData AnySecp256k1PublicKey where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData d = case unsafeFromBuiltinData d of
    bs
      | P.lengthOfByteString bs P.== (33 :: Integer) ->
          AnyCompressedSecp256k1PublicKey (CompressedSecp256k1PublicKey bs)
    bs
      | P.lengthOfByteString bs P.== (64 :: Integer) ->
          AnySecp256k1PublicKey (Secp256k1PublicKey bs)
    _ -> P.error ()

instance FromData AnySecp256k1PublicKey where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData d =
    fromBuiltinData d >>= \case
      bs
        | P.lengthOfByteString bs P.== (33 :: Integer) ->
            Just $ AnyCompressedSecp256k1PublicKey (CompressedSecp256k1PublicKey bs)
      bs
        | P.lengthOfByteString bs P.== (64 :: Integer) ->
            Just $ AnySecp256k1PublicKey (Secp256k1PublicKey bs)
      _ -> Nothing

instance ToData AddressBech32 where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData = toBuiltinData . AddressBech32.rawBytes

instance UnsafeFromData AddressBech32 where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData d = case AddressBech32.parse $ unsafeFromBuiltinData d of
    Just ab -> ab
    Nothing -> P.error ()

instance FromData AddressBech32 where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = (=<<) AddressBech32.parse . fromBuiltinData

makeIsDataIndexed
  ''SolanaSignature
  [('SolanaVerbatim, 0), ('SolanaCLIFormat, 1), ('SolanaOffchainMessage, 2)]
makeIsDataIndexed
  ''BitcoinScriptPubKeyInfo
  [('P2TRPubKey, 0), ('P2WPKHPubKey, 1), ('P2PKHPubKey, 2)]
makeIsDataIndexed
  ''SighashFlag
  [('SIGHASH_ALL, 0), ('SIGHASH_DEFAULT, 1)]
makeIsDataIndexed ''BitcoinSigningScheme [('BIP0137, 0), ('BIP0322, 1)]
makeIsDataIndexed
  ''CIP8ProtectedHeaderInfo
  [('NonStandardProtectedHeader, 0), ('StandardProtectedHeader, 1)]
makeIsDataIndexed
  ''CardanoSigningScheme
  [('FakeTransactionSigningScheme, 0), ('CIP8SigningScheme, 1)]
makeIsDataIndexed
  ''VerifiableClaimCredential
  [ ('CardanoShelleyCredential, 0)
  , ('CardanoByronCredential, 1)
  , ('BitcoinCredential, 2)
  , ('EthereumCredential, 3)
  , ('BnbCredential, 4)
  , ('BatCredential, 5)
  , ('AvaxCredential, 6)
  , ('BaseCredential, 7)
  , ('SolanaCredential, 8)
  , ('XrpCredential, 9)
  ]

instance Arbitrary AddressBech32 where
  arbitrary = do
    network <- arbitrary
    Address paymentCredential possibleStakingCredentials <- arbitrary
    -- Check detailed comment in `AddressBech32` about the checksum length
    checksumBytes <- genHash 5
    -- If we have non pointer staking credentials we need to generate a valid address
    let paymentCredentialBytes = case paymentCredential of
          ScriptCredential (ScriptHash bytes) -> bytes
          PubKeyCredential (PubKeyHash bytes) -> bytes
        stakingCredentialBytes = case possibleStakingCredentials of
          Just (StakingHash (ScriptCredential (ScriptHash bytes))) -> bytes
          Just (StakingHash (PubKeyCredential (PubKeyHash bytes))) -> bytes
          _ -> ""
        addrTypeNibbleValue :: Integer
        addrTypeNibbleValue = case (paymentCredential, possibleStakingCredentials) of
          (PubKeyCredential _, Just (StakingHash (PubKeyCredential _))) -> 0
          (ScriptCredential _, Just (StakingHash (PubKeyCredential _))) -> 1
          (PubKeyCredential _, Just (StakingHash (ScriptCredential _))) -> 2
          (ScriptCredential _, Just (StakingHash (ScriptCredential _))) -> 3
          (PubKeyCredential _, _) -> 6
          (ScriptCredential _, _) -> 7
        headerByte = case network of
          AddressBech32.Testnet -> P.integerToByteString BigEndian 1 (addrTypeNibbleValue * 16)
          AddressBech32.Mainnet -> P.integerToByteString BigEndian 1 (addrTypeNibbleValue * 16 + 1)
        rawBytes =
          headerByte
            `P.appendByteString` paymentCredentialBytes
            `P.appendByteString` stakingCredentialBytes
            `P.appendByteString` checksumBytes
    return $
      unsafeAddressBech32
        (AddressBech32.HeaderType addrTypeNibbleValue)
        network
        rawBytes

instance CoArbitrary AddressBech32 where
  coarbitrary = genericCoarbitrary

instance Arbitrary AnySecp256k1PublicKey where
  arbitrary =
    oneof
      [ AnySecp256k1PublicKey <$> arbitrary
      , AnyCompressedSecp256k1PublicKey <$> arbitrary
      ]
  shrink = genericShrink

instance CoArbitrary AnySecp256k1PublicKey where
  coarbitrary = genericCoarbitrary

instance Arbitrary SighashFlag where
  arbitrary = elements [SIGHASH_ALL, SIGHASH_DEFAULT]
  shrink = genericShrink

instance CoArbitrary SighashFlag where
  coarbitrary = genericCoarbitrary

instance Arbitrary BitcoinSigningScheme where
  arbitrary =
    arbitrary >>= \case
      True ->
        BIP0137
          <$> arbitrary
      False -> BIP0322 <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance CoArbitrary BitcoinSigningScheme where
  coarbitrary = genericCoarbitrary

instance Arbitrary SchnorrSecp256k1PublicKey where
  arbitrary = SchnorrSecp256k1PublicKey <$> genHash 32
  shrink = fmap SchnorrSecp256k1PublicKey . shrinkHash . getSchnorrSecp256k1PublicKey

instance CoArbitrary SchnorrSecp256k1PublicKey where
  coarbitrary = genericCoarbitrary

instance Arbitrary BitcoinScriptPubKeyInfo where
  arbitrary =
    elements [0, 1, 3 :: Int] >>= \case
      0 -> P2TRPubKey <$> arbitrary
      1 -> P2WPKHPubKey <$> arbitrary
      _ -> P2PKHPubKey <$> arbitrary
  shrink = genericShrink

instance CoArbitrary BitcoinScriptPubKeyInfo where
  coarbitrary = genericCoarbitrary

instance Arbitrary SolanaSignature where
  arbitrary =
    elements [0, 1, 2 :: Int] >>= \case
      0 -> pure SolanaVerbatim
      1 -> pure SolanaCLIFormat
      _ -> pure SolanaOffchainMessage
  shrink = genericShrink

instance CoArbitrary SolanaSignature where
  coarbitrary = genericCoarbitrary

instance Arbitrary CIP8ProtectedHeaderInfo where
  arbitrary =
    oneof
      [ NonStandardProtectedHeader <$> arbitrary
      , StandardProtectedHeader <$> arbitrary
      ]
  shrink = genericShrink

instance CoArbitrary CIP8ProtectedHeaderInfo where
  coarbitrary = genericCoarbitrary

instance Arbitrary CardanoSigningScheme where
  arbitrary =
    elements [True, False] >>= \case
      True -> pure FakeTransactionSigningScheme
      False -> CIP8SigningScheme <$> arbitrary
  shrink = genericShrink

instance CoArbitrary CardanoSigningScheme where
  coarbitrary = genericCoarbitrary

instance Arbitrary VerifiableClaimCredential where
  arbitrary =
    oneof
      [ CardanoShelleyCredential
          <$> arbitrary
          <*> arbitrary
      , CardanoByronCredential
          <$> arbitrary
          <*> arbitrary
          <*> arbitrary
      , BitcoinCredential <$> arbitrary
      , EthereumCredential <$> arbitrary
      , BnbCredential <$> arbitrary
      , BatCredential <$> arbitrary
      , AvaxCredential <$> arbitrary
      , BaseCredential <$> arbitrary
      , SolanaCredential <$> arbitrary <*> arbitrary
      , XrpCredential <$> arbitrary
      ]
  shrink = genericShrink

instance CoArbitrary VerifiableClaimCredential where
  coarbitrary = genericCoarbitrary

instance Arbitrary Bip32ChainCode where
  arbitrary = Bip32ChainCode <$> genHash 32
  shrink = fmap Bip32ChainCode . shrinkHash . getBip32ChainCode

instance CoArbitrary Bip32ChainCode where
  coarbitrary = genericCoarbitrary

deriving newtype instance Arbitrary AddressAttributes
deriving newtype instance CoArbitrary AddressAttributes
deriving newtype instance Arbitrary Signature
deriving newtype instance CoArbitrary Signature

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map k v) where
  arbitrary = AMap.unsafeFromList . Map.toAscList <$> arbitrary
  shrink =
    fmap (AMap.unsafeFromList . Map.toAscList) . shrink . Map.fromList . AMap.toList

instance (CoArbitrary k, CoArbitrary v) => CoArbitrary (Map k v) where
  coarbitrary = genericCoarbitrary

deriving via (Positive Integer) instance (Arbitrary Lovelace)

instance CoArbitrary Star where
  coarbitrary = genericCoarbitrary

instance CoArbitrary POSIXTime where
  coarbitrary = genericCoarbitrary

instance CoArbitrary DiffMilliSeconds where
  coarbitrary = genericCoarbitrary

instance Arbitrary StakingCredential where
  arbitrary = StakingHash <$> arbitrary
  shrink = genericShrink

instance CoArbitrary StakingCredential where
  coarbitrary = genericCoarbitrary

instance Arbitrary Credential where
  arbitrary = oneof [PubKeyCredential <$> arbitrary, ScriptCredential <$> arbitrary]
  shrink = genericShrink

instance CoArbitrary Credential where
  coarbitrary = genericCoarbitrary

instance Arbitrary Address where
  arbitrary = Address <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance CoArbitrary Address where
  coarbitrary = genericCoarbitrary

instance Arbitrary ScriptHash where
  arbitrary = ScriptHash <$> genHash 28
  shrink = fmap ScriptHash . shrinkHash . getScriptHash

instance CoArbitrary ScriptHash where
  coarbitrary = genericCoarbitrary

instance Arbitrary PubKeyHash where
  arbitrary = PubKeyHash <$> genHash 28
  shrink = fmap PubKeyHash . shrinkHash . getPubKeyHash

instance CoArbitrary PubKeyHash where
  coarbitrary = genericCoarbitrary

instance Arbitrary MerkleHash where
  arbitrary = MerkleHash <$> genHash 32
  shrink = fmap MerkleHash . shrinkHash . getMerkleHash

instance CoArbitrary MerkleHash where
  coarbitrary = genericCoarbitrary

instance Arbitrary Ed25519PublicKey where
  arbitrary = Ed25519PublicKey <$> genHash 32
  shrink = fmap Ed25519PublicKey . shrinkHash . getEd25519PublicKey

instance CoArbitrary Ed25519PublicKey where
  coarbitrary = genericCoarbitrary

instance Arbitrary Secp256k1PublicKey where
  arbitrary = Secp256k1PublicKey <$> genHash 64
  shrink = fmap Secp256k1PublicKey . shrinkHash . getSecp256k1PublicKey

instance CoArbitrary Secp256k1PublicKey where
  coarbitrary = genericCoarbitrary

instance Arbitrary Ed25519orSecp256k1PublicKey where
  arbitrary =
    oneof
      [ Ed25519orSecp256k1PublicKey <$> genHash 32
      , Ed25519orSecp256k1PublicKey <$> genHash 64
      ]
  shrink =
    fmap Ed25519orSecp256k1PublicKey . shrinkHash . getEd25519orSecp256k1PublicKey

instance CoArbitrary Ed25519orSecp256k1PublicKey where
  coarbitrary = genericCoarbitrary

instance Arbitrary CompressedSecp256k1PublicKey where
  arbitrary = CompressedSecp256k1PublicKey <$> genHash 33
  shrink =
    fmap CompressedSecp256k1PublicKey . shrinkHash . getCompressedSecp256k1PublicKey

instance CoArbitrary CompressedSecp256k1PublicKey where
  coarbitrary = genericCoarbitrary

instance Arbitrary TokenName where
  arbitrary = do
    len <- chooseInt (0, 32)
    TokenName . toBuiltin . BS.pack <$> replicateM len arbitrary
  shrink =
    fmap (TokenName . toBuiltin . BS.pack)
      . shrink
      . BS.unpack
      . fromBuiltin
      . unTokenName

instance Arbitrary CurrencySymbol where
  arbitrary = CurrencySymbol <$> genHash 28
  shrink = fmap CurrencySymbol . shrinkHash . unCurrencySymbol

instance CoArbitrary CurrencySymbol where
  coarbitrary = genericCoarbitrary

instance CoArbitrary TokenName where
  coarbitrary = genericCoarbitrary

instance Arbitrary BuiltinByteString where
  arbitrary = toBuiltin . BS.pack <$> arbitrary
  shrink = fmap (toBuiltin . BS.pack) . shrink . BS.unpack . fromBuiltin

instance CoArbitrary BuiltinByteString where
  coarbitrary = coarbitrary . BS.unpack . fromBuiltin

shrinkHash :: BuiltinByteString -> [BuiltinByteString]
shrinkHash = fmap (toBuiltin . BS.pack) . shrinkElements . BS.unpack . fromBuiltin

shrinkElements :: (Arbitrary a) => [a] -> [[a]]
shrinkElements [] = []
shrinkElements (a : as) = go [] a as
  where
    go pre item post = case post of
      [] -> itemShrinks
      item' : post' -> itemShrinks <> go (item : pre) item' post'
      where
        itemShrinks = do
          item' <- shrink item
          pure $ reverse pre <> (item' : post)

genHash :: Int -> Gen BuiltinByteString
genHash size = toBuiltin . BS.pack <$> replicateM size arbitrary

instance Arbitrary TxId where
  arbitrary = TxId <$> genHash 32
  shrink = fmap TxId . shrinkHash . getTxId

instance CoArbitrary TxId where
  coarbitrary = genericCoarbitrary

instance Arbitrary TxOutRef where
  arbitrary = TxOutRef <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance CoArbitrary TxOutRef where
  coarbitrary = genericCoarbitrary
