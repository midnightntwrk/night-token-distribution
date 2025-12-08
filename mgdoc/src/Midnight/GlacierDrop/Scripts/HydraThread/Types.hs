{-# LANGUAGE TemplateHaskell #-}

module Midnight.GlacierDrop.Scripts.HydraThread.Types where

import Data.Eq (Eq)
import GHC.Generics (Generic)
import Midnight.GlacierDrop.Api (
  MerkleHash,
  Sha512HalfScriptHash,
  Signature,
  Star,
  VerifiableClaimCredential (..),
 )
import Midnight.GlacierDrop.Scripts.RedemptionSupply.Types (MerkleTreeDepth)
import qualified Midnight.GlacierDrop.Scripts.Supply.Cardano.AddressBech32 as AddressBech32
import PlutusLedgerApi.V3 (Credential, PubKeyHash, ScriptHash, TxId)
import PlutusTx
import PlutusTx.Prelude hiding (Eq, error)
import Refined (Ascending)
import Text.Show (Show)

data HydraThreadParams = HydraThreadParams
  { hydraCommitScriptHash :: ScriptHash
  -- ^ The script hash of the Hydra vCommit script.
  , messagePrefix :: BuiltinByteString
  -- ^ The prefix of the off-chain message
  , messageSuffix :: BuiltinByteString
  -- ^ The suffix of the off-chain message
  , tgeAgentAuthKeys :: Ascending [PubKeyHash]
  -- ^ The authorization pub key hashes of the TGE agent.
  , minAuthSignatures :: Integer
  -- ^ The minimum number of TGE signatures required to perform privileged
  -- actions.
  , sha512HalfScriptHash :: Sha512HalfScriptHash
  -- ^ The script hash of the reward script which will be
  -- used to proof the pub key and pub key hash correspondence.
  -- claims.
  , trieInsertCred :: Credential
  -- ^ The ScriptCredential of the FFI `trieInsert` validator.
  }
  deriving (Show)

makeLift ''HydraThreadParams

data HydraThreadClaiming = MakeHydraThreadClaiming
  { allotmentsTreeRoot :: MerkleHash
  -- ^ The hash of the root node of the allocation merkle tree.
  , allotmentsTreeDepth :: Integer
  , alreadyClaimedTrieRoot :: MerkleHash
  -- ^ The root hash of the set (represented as trie) which marks already claimed allotments.
  , redemptionsSum :: Star
  -- ^ The sum of star claimed.
  }
  deriving (Eq, Show, Generic)

makeIsDataIndexed
  ''HydraThreadClaiming
  [('MakeHydraThreadClaiming, 0)]

data HydraThreadFinal = MakeHydraThreadFinal
  { finalTransactionId :: TxId
  , redemptionsTreeRoot :: MerkleHash
  , redemptionsTreeDepth :: MerkleTreeDepth
  , redemptionsSum :: Star
  }
  deriving (Eq, Show, Generic)

makeIsDataIndexed
  ''HydraThreadFinal
  [('MakeHydraThreadFinal, 0)]

data HydraThreadDatum
  = HydraThreadClaiming HydraThreadClaiming
  | HydraThreadFinal HydraThreadFinal
  deriving (Eq, Show, Generic)

makeIsDataIndexed
  ''HydraThreadDatum
  [('HydraThreadClaiming, 0), ('HydraThreadFinal, 1)]

data NewDestination = NewDestination
  deriving (Show, Eq, Generic)

makeIsDataIndexed
  ''NewDestination
  [('NewDestination, 0)]

data ClaimInfo = ClaimInfo
  { amount :: Star
  , from :: VerifiableClaimCredential
  , to :: AddressBech32.AddressBech32
  , signature :: Signature
  , allotmentsIndex :: Integer
  , allotmentsTreeProof :: [MerkleHash]
  , alreadyClaimedTrieProof :: BuiltinData
  , alreadyClaimedTrieRoot :: Maybe MerkleHash
  , alreadyClaimedTrieRootNew :: MerkleHash
  }
  deriving stock (Show, Eq, Generic)

makeIsDataIndexed ''ClaimInfo [('ClaimInfo, 0)]

data HydraThreadRedeemer
  = HydraThreadEnter
  | HydraThreadClaim ClaimInfo
  | HydraThreadFinalize MerkleHash MerkleTreeDepth
  | -- | Runs alongside a NightSpendGenesis transaction on Cardano.
    --
    --   Requires:
    --   - The spent input contains 1 token with the hydra thread token name.
    --   - There is a minting redeemer for the currency symbol that matches the input token.
    --
    --   NOTE These checks are sufficient to ensure the Night script enforces
    --   all other invariants.
    HydraThreadGenesis
  deriving (Eq, Show, Generic)

makeIsDataIndexed
  ''HydraThreadRedeemer
  [ ('HydraThreadClaim, 0)
  , ('HydraThreadGenesis, 1)
  , ('HydraThreadEnter, 2)
  , ('HydraThreadFinalize, 3)
  ]
