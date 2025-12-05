{-# LANGUAGE InstanceSigs #-}

module Midnight.GlacierDrop.Scripts.MerkleTrieFFI.Types (
  PTrieInsertRedeemer (..),
  TrieInsertRedeemer (..),
  MerklePatriciaForestry (..),
  ProofStep (..),
  Neighbor (..),
) where

import GHC.Generics (Generic)
import qualified Generics.SOP as SOP
import Plutarch.MerkleTree.PatriciaForestry hiding (MerklePatriciaForestry (..))
import Plutarch.Prelude
import PlutusLedgerApi.V3
import qualified PlutusTx
import qualified PlutusTx.Builtins.Internal as BI
import qualified PlutusTx.Prelude as PlutusTx
import Prelude

newtype MerklePatriciaForestry = MerklePatriciaForestry BuiltinByteString
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

-- >>> PlutusTx.toBuiltinData $ MerklePatriciaForestry "deadbeef"
-- B "deadbeef"

data TrieInsertRedeemer = TrieInsertRedeemer
  { outputRoot :: MerklePatriciaForestry
  , inputRoot :: MerklePatriciaForestry
  , inputKey :: BuiltinByteString
  , inputValue :: BuiltinByteString
  , inputProof :: [ProofStep]
  }
  deriving stock (Show, Eq, Generic)

instance PlutusTx.ToData TrieInsertRedeemer where
  toBuiltinData TrieInsertRedeemer{outputRoot, inputRoot, inputKey, inputValue, inputProof} =
    let inputRootData = PlutusTx.toBuiltinData inputRoot
        inputKeyData = PlutusTx.toBuiltinData inputKey
        inputValueData = PlutusTx.toBuiltinData inputValue
        inputProofData = PlutusTx.toBuiltinData inputProof
        outputRootData = PlutusTx.toBuiltinData outputRoot
     in BI.mkList $
          BI.mkCons
            outputRootData
            ( BI.mkCons
                inputRootData
                ( BI.mkCons
                    inputKeyData
                    (BI.mkCons inputValueData (BI.mkCons inputProofData (BI.mkNilData BI.unitval)))
                )
            )

instance PlutusTx.FromData TrieInsertRedeemer where
  fromBuiltinData :: PlutusTx.BuiltinData -> Maybe TrieInsertRedeemer
  fromBuiltinData builtinData = do
    xs <-
      BI.chooseData
        builtinData
        Nothing
        Nothing
        (Just $ BI.unsafeDataAsList builtinData)
        Nothing
        Nothing
    outputRoot_ <- PlutusTx.fromBuiltinData $ BI.head xs
    let tail_ = BI.tail xs
    inputRoot_ <- PlutusTx.fromBuiltinData $ BI.head tail_
    let tail_2 = BI.tail tail_
    inputKey_ <- PlutusTx.fromBuiltinData $ BI.head tail_2
    let tail_3 = BI.tail tail_2
    inputValue_ <- PlutusTx.fromBuiltinData $ BI.head tail_3
    let tail_4 = BI.tail tail_3
    inputProof_ <- PlutusTx.fromBuiltinData $ BI.head tail_4
    PlutusTx.pure
      PlutusTx.$ TrieInsertRedeemer outputRoot_ inputRoot_ inputKey_ inputValue_ inputProof_

data PTrieInsertRedeemer (s :: S) = PTrieInsertRedeemer
  { ptiOutputRoot :: Term s (PAsData PMerklePatriciaForestry)
  , ptiInputRoot :: Term s (PAsData PMerklePatriciaForestry)
  , ptiInputKey :: Term s (PAsData PByteString)
  , ptiInputValue :: Term s (PAsData PByteString)
  , ptiInputProof :: Term s (PAsData PProof)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PShow, PEq)
  deriving (PlutusType) via (DeriveAsDataRec PTrieInsertRedeemer)

deriving via
  DeriveDataPLiftable (PAsData PTrieInsertRedeemer) TrieInsertRedeemer
  instance
    PLiftable PTrieInsertRedeemer
