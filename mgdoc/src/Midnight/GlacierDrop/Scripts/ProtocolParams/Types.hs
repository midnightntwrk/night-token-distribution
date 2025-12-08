{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Midnight.GlacierDrop.Scripts.ProtocolParams.Types (
  NightMintingParams (..),
  UpgradeParamsAction (..),
  nightProtocolParamsToken,
) where

import GHC.Generics (Generic)
import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.Builtins.HasOpaque qualified as BI
import Prelude

-- We are using `dynamicMintingLogic` as a `BuiltinData` to avoid
-- decoding and reencoding the `Credential` during withdrawal credential
-- checking - we are during direct builtin data comparison there.
-- This path is highly optimized because it will be possibly used massively on the chain
-- even by external users. We can imagine in the future burn or other open upgrade action.
-- There is an extra assumption that the policies included withdrawals
-- set could be actually larger then the one used by us in this context.[:walletName
data NightMintingParams = NightMintingParams
  { dynamicMintingLogic :: BuiltinData
  -- ^ night minting policy upgradeable logic
  -- builtin-data encoded `Credential`
  , hydraThreadScriptHash :: ScriptHash
  -- ^ The hash of the hydra thread script.
  , reserveScriptAddress :: Address
  -- ^ address of the reserve script
  , supplyScriptAddress :: Address
  -- ^ address of the supply script
  , foundationWallets :: [(Address, Integer)]
  -- ^ An associative list of Cardano addresses that should receive a predetermined amount of
  -- STAR.
  , tgeAgentAuthKeys :: [PubKeyHash]
  -- ^ The authorization pub key hashes of the TGE agent.
  , minAuthSignatures :: Integer
  -- ^ The minimum number of TGE signatures required to perform privileged
  -- actions.
  }
  deriving stock (Generic)
PlutusTx.makeIsDataIndexed
  ''NightMintingParams
  [('NightMintingParams, 0)]

data UpgradeParamsAction = UpdateParameters | FullUpgrade
  deriving stock (Generic)
PlutusTx.makeIsDataIndexed
  ''UpgradeParamsAction
  [('UpdateParameters, 0), ('FullUpgrade, 1)]

nightProtocolParamsToken :: TokenName
nightProtocolParamsToken = TokenName $ BI.stringToBuiltinByteStringUtf8 "NightProtocolParams"
