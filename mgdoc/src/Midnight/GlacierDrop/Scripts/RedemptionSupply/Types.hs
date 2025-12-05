{-# LANGUAGE TemplateHaskell #-}

module Midnight.GlacierDrop.Scripts.RedemptionSupply.Types (
  MaterializationInfo (..),
  MaterializationMask (..),
  MerkleTreeDepth (..),
  RedemptionShardDepth (..),
  RedemptionSupplyDatum (..),
  RedemptionSupplyState (..),
  RedemptionSupplyParams (..),
  RedemptionSupplyRedeemer (..),
  SubdivisionDepth (..),
  TotalIncrements (..),
) where

import GHC.Generics qualified as H
import Midnight.GlacierDrop.Api (MerkleHash (..))
import Midnight.GlacierDrop.Scripts.ThawingSchedule (
  GenesisTimestamp (..),
  RedemptionIncrementPeriod,
 )
import PlutusLedgerApi.V3 qualified as P
import PlutusTx (makeIsDataIndexed, makeLift)
import PlutusTx.Prelude hiding (Eq, error)
import PlutusTx.Prelude qualified as P
import Prelude qualified as H

newtype MerkleTreeDepth = MerkleTreeDepth {getMerkleTreeDepth :: Integer}
  deriving (H.Eq, H.Show, H.Ord, H.Generic)
  deriving newtype (P.Eq, P.Ord, P.UnsafeFromData, P.FromData, P.ToData)

makeLift ''MerkleTreeDepth

newtype RedemptionShardDepth = RedemptionShardDepth MerkleTreeDepth
  deriving (H.Eq, H.Show)
  deriving newtype (P.Eq, P.Ord, P.UnsafeFromData, P.FromData, P.ToData)

makeLift ''RedemptionShardDepth

newtype TotalIncrements = TotalIncrements {getTotalIncrements :: Integer}
  deriving (H.Show)
  deriving newtype (H.Eq, P.Eq, P.Ord, P.UnsafeFromData, P.FromData, P.ToData)

makeLift ''TotalIncrements

data RedemptionSupplyParams = RedemptionSupplyParams
  { redemptionScriptHash :: P.ScriptHash
  -- ^ The script hash of the redemption script.
  , jitterStrataCount :: Integer
  -- ^ The number of strata across which to jitter the thaw schedules of
  -- claims.
  , redemptionShardDepth :: RedemptionShardDepth
  -- ^ The desired depth of the subdivision tree.
  , redemptionIncrements :: TotalIncrements
  -- ^ The total number of thaw increments.
  , tgeWalletAddress :: P.Address
  -- ^ The wallet address to send the collected funds.
  }
  deriving (H.Show)

makeLift ''RedemptionSupplyParams

newtype MaterializationMask = MaterializationMask BuiltinByteString
  deriving (H.Eq, H.Show, H.Ord)
  deriving newtype (P.UnsafeFromData, P.FromData, P.ToData, P.Eq)

data RedemptionSupplyState
  = Subdividing MerkleTreeDepth
  | Materializing MaterializationMask
  deriving (H.Eq, H.Show)

makeIsDataIndexed
  ''RedemptionSupplyState
  [('Subdividing, 0), ('Materializing, 1)]

data RedemptionSupplyDatum = RedemptionSupplyDatum
  { rootHash :: MerkleHash
  , genesisTimestamp :: GenesisTimestamp
  , redemptionIncrementPeriod :: RedemptionIncrementPeriod
  -- ^ The duration of the period between thaw increments.
  , state :: RedemptionSupplyState
  }
  deriving (H.Eq, H.Show)

makeIsDataIndexed ''RedemptionSupplyDatum [('RedemptionSupplyDatum, 0)]

{- Note [Merkle tree schema]

leaf_digest:
  4 bytes of jitter stratum as a 32-bit big endian integer
  8 bytes of amount as a 64-bit big endian integer
  raw address bytes (variable length) (either 28 or 56 bytes of credentials - we don't support pointers)

leaf_hash:
  sha_256(leaf_digest)

node_digest:
  32 bytes of left node hash
  8 bytes of left amount as a 64-bit big endian integer
  32 bytes of right node hash
  8 bytes of right amount as a 64-bit big endian integer

node_hash:
  sha_256(node_digest)

-}

data MaterializationInfo = MaterializationInfo
  { jitterStratum :: Integer
  -- ^ The jitter stratum assigned to this claim.
  , amount :: Integer
  -- ^ The amount of STAR tokens claimed.
  , membershipProof :: [(MerkleHash, Integer)]
  -- ^ The bottom-up list of sibling hashes and totals all the way to the subtree root.
  , claimIndex :: Integer
  -- ^ The index of the claim within the subtree.
  }
  deriving (H.Eq, H.Show, H.Generic)

makeIsDataIndexed ''MaterializationInfo [('MaterializationInfo, 0)]

-- How many levels we are trying to subdivide into.
newtype SubdivisionDepth = SubdivisionDepth Integer
  deriving (H.Eq, H.Show)
  deriving newtype (P.UnsafeFromData, P.FromData, P.ToData)

data RedemptionSupplyRedeemer
  = RedemptionSupplySubdivide
      SubdivisionDepth
      -- ^ the number of subdivisions
      [MerkleHash]
      -- ^ the empty subtree hashes
  | -- | A materialize transaction marks a claim as materialized and materializes a redemption script output.
    --
    --   Requires:
    --   - There is only 1 script input, the spent input.
    --   - All other inputs are ADA-only.
    --   - The spent input only contains ADA and STAR.
    --   - The claim index is not set in the input materializationMask
    --   - The input proof is valid
    --   - outputs[0].address = input.address
    --   - outputs[0].value = input.value
    --   - outputs[0].datum.rootHash = datum.rootHash
    --   - outputs[0].datum.materializationMask = datum.materializationMask .|. (1 .<<. claimInfo.claimIndex)
    --   - outputs[1].address = params.claimScriptHash
    --   - outputs[1].value only has STAR and ADA
    --   - outputs[1].value.star == redeemer.amount
    --   - outputs[1].datum.destinationAddress = redeemer.destinationAddress
    --   - outputs[1].datum.nextThaw = datum.genesisTimestamp + (params.redemptionIncrementPeriod * redeemer.jitterStratum / params.jitterStrataCount)
    --   - outputs[1].datum.incrementsLeft = params.redemptionIncrements
    RedemptionSupplyMaterialize MaterializationInfo
  | -- | A collect transaction collects redemptin supply utxos that are empty and no one can materialize anymore.
    --
    --   Requires:
    --   - The spent input only contains ADA.
    --   - The transaction is signed by the min number of tge auth keys.
    --   - The return address is the specified tge payment key
    RedemptionSupplyCollect
  deriving (H.Eq, H.Show)

makeIsDataIndexed
  ''RedemptionSupplyRedeemer
  [ ('RedemptionSupplySubdivide, 0)
  , ('RedemptionSupplyMaterialize, 1)
  , ('RedemptionSupplyCollect, 2)
  ]
