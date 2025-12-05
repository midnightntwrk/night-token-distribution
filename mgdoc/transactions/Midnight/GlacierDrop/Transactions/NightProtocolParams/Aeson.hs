{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Midnight.GlacierDrop.Transactions.NightProtocolParams.Aeson where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.=))
import Data.Aeson qualified as JSON
import Midnight.GlacierDrop.Contrib.Data.Aeson (HexEncoded (HexEncoded))
import Midnight.GlacierDrop.Scripts.ProtocolParams.Types (
  NightMintingParams (..),
 )
import PlutusLedgerApi.V3 qualified as PV3
import PlutusTx qualified
import PlutusTx.Prelude qualified as PlutusTx
import Prelude

-- JSON instances for PlutusTx.BuiltinByteString
instance ToJSON PlutusTx.BuiltinByteString where
  toJSON = toJSON . HexEncoded . PlutusTx.fromBuiltin

instance FromJSON PlutusTx.BuiltinByteString where
  parseJSON v = do
    HexEncoded bs <- parseJSON v
    pure $ PlutusTx.toBuiltin bs

-- JSON instances for Plutus types
deriving anyclass instance ToJSON PV3.Credential
deriving anyclass instance FromJSON PV3.Credential

deriving anyclass instance ToJSON PV3.Address
deriving anyclass instance FromJSON PV3.Address

deriving anyclass instance ToJSON PV3.PubKeyHash
deriving anyclass instance FromJSON PV3.PubKeyHash

deriving anyclass instance ToJSON PV3.StakingCredential
deriving anyclass instance FromJSON PV3.StakingCredential

deriving anyclass instance ToJSON PV3.ScriptHash
deriving anyclass instance FromJSON PV3.ScriptHash

-- Eq and Show instances for NightMintingParams
instance Eq NightMintingParams where
  (==) a b = PlutusTx.toBuiltinData a == PlutusTx.toBuiltinData b

instance Show NightMintingParams where
  show _ = "NightMintingParams{...}"

-- Not a proper ToJSON instance because it can fail
nightMintingParamsToJSON :: NightMintingParams -> Maybe JSON.Value
nightMintingParamsToJSON (NightMintingParams{..}) = do
  -- Sanity check
  (dynamicMintingLogicCredential :: PV3.Credential) <-
    PlutusTx.fromBuiltinData dynamicMintingLogic
  case dynamicMintingLogicCredential of
    PV3.ScriptCredential _ -> pure ()
    _ -> Nothing
  pure $
    JSON.object
      [ "dynamicMintingLogic" .= dynamicMintingLogicCredential
      , "hydraThreadScriptHash" .= hydraThreadScriptHash
      , "reserveScriptAddress" .= reserveScriptAddress
      , "supplyScriptAddress" .= supplyScriptAddress
      , "foundationWallets" .= foundationWallets
      , "tgeAgentAuthKeys" .= tgeAgentAuthKeys
      , "minAuthSignatures" .= minAuthSignatures
      ]

instance FromJSON NightMintingParams where
  parseJSON = withObject "NightMintingParams" $ \obj -> do
    (dynamicMintingLogicCredential :: PV3.Credential) <-
      obj JSON..: "dynamicMintingLogic"
    hydraThreadScriptHash <- obj JSON..: "hydraThreadScriptHash"
    reserveScriptAddress <- obj JSON..: "reserveScriptAddress"
    supplyScriptAddress <- obj JSON..: "supplyScriptAddress"
    foundationWallets <- obj JSON..: "foundationWallets"
    tgeAgentAuthKeys <- obj JSON..: "tgeAgentAuthKeys"
    minAuthSignatures <- obj JSON..: "minAuthSignatures"

    pure $
      NightMintingParams
        { dynamicMintingLogic = PlutusTx.toBuiltinData dynamicMintingLogicCredential
        , hydraThreadScriptHash
        , reserveScriptAddress
        , supplyScriptAddress
        , foundationWallets
        , tgeAgentAuthKeys
        , minAuthSignatures
        }
