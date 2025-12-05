{-# LANGUAGE TemplateHaskell #-}

module Midnight.GlacierDrop.Scripts.Night.Types where

import Data.Eq (Eq)
import Midnight.GlacierDrop.Api (MerkleHash)
import Midnight.GlacierDrop.Scripts.ThawingSchedule (RedemptionIncrementPeriod)
import PlutusLedgerApi.V3 (
  CurrencySymbol,
  POSIXTime,
  ScriptHash,
  TxOutRef,
 )
import PlutusTx (makeIsDataIndexed, makeLift)
import PlutusTx.Prelude hiding (Eq, error)
import Text.Show (Show)

data NightParams = NightParams
  { merkleRoot :: MerkleHash
  -- ^ The hash of the root node of the allocation merkle tree.
  , treeDepth :: Integer
  -- ^ The depth of the allocation merkle tree.
  , seedInput :: TxOutRef
  -- ^ The seed input that must be consumed by the STAR minting transaction to
  -- ensure uniqueness.
  -- ^ The authorization pub key hashes of the TGE agent.
  , minTreasuryTokens :: Integer
  -- ^ The minimum number of tokens that should be sent to the treasury on
  -- reallocation. If there are insufficient tokens to distribute between the
  -- foundation wallets and the treasury, the foundation wallet amounts are
  -- scaled down.
  , supply :: Integer
  -- ^ The total number of STAR tokens to be minted.
  , allocationCount :: Integer
  -- ^ The number of allocations in the allocation tree.
  , jitterStrataCount :: Integer
  -- ^ The number of strata across which to jitter the thaw schedules of
  -- claims.
  , protocolParamsCS :: CurrencySymbol
  -- ^ The currency symbol of the protocol parameters NFT.
  }
  deriving (Show)

makeLift ''NightParams

-- NOTE(jamie): No Datum

data NightGenesis = NightGenesis
  { treasuryScriptHash :: ScriptHash
  -- ^ The hash of the script that holds the Midnight treasury and reserve.
  , genesisTimestamp :: POSIXTime
  -- ^ The timestamp of the Midnight genesis block.
  , redemptionIncrementPeriod :: RedemptionIncrementPeriod
  -- ^ The duration of the period between thaw increments.
  }
  deriving (Eq, Show)

makeIsDataIndexed ''NightGenesis [('NightGenesis, 0)]

data NightMintRedeemer
  = NightMintStar
  | NightBurnGenesis NightGenesis
  | RemintHydraThread
  | DynamicMintingLogic
  deriving (Eq, Show)

makeIsDataIndexed
  ''NightMintRedeemer
  [ ('NightMintStar, 0)
  , ('NightBurnGenesis, 1)
  , ('RemintHydraThread, 2)
  , ('DynamicMintingLogic, 3)
  ]
