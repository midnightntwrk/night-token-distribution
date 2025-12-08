{-# LANGUAGE TemplateHaskell #-}

module Midnight.GlacierDrop.Scripts.NightCustom.Types where

import Data.Eq (Eq)
import PlutusLedgerApi.V3 (
  CurrencySymbol,
  POSIXTime,
 )
import PlutusTx (makeLift)
import Text.Show (Show)

data NightCustomParams = NightCustomParams
  { protocolParamsCS :: CurrencySymbol
  -- ^ The currency symbol of the protocol parameters NFT.
  , policyId :: CurrencySymbol
  -- ^ The currency symbol of the NIGHT tokens.
  , genesisTimestamp :: POSIXTime
  -- ^ The timestamp of the Midnight genesis block.
  }
  deriving (Eq, Show)

makeLift ''NightCustomParams
