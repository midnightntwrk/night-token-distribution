{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Midnight.GlacierDrop.Scripts.MerkleTrieFFI.Terms where

import Midnight.GlacierDrop.Scripts.MerkleTrieFFI.Types
import Plutarch.Core.Integrity (pisRewardingScript)
import Plutarch.Core.ValidationLogic (pvalidateConditions)
import Plutarch.LedgerApi.V3
import Plutarch.MerkleTree.PatriciaForestry
import qualified Plutarch.Monadic as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import Prelude

mkInsertTrieValidator :: ClosedTerm (PScriptContext :--> PUnit)
mkInsertTrieValidator = plam $ \ctx -> P.do
  PScriptContext{pscriptContext'redeemer, pscriptContext'scriptInfo} <-
    pmatch ctx
  PTrieInsertRedeemer
    { ptiOutputRoot
    , ptiInputRoot
    , ptiInputKey
    , ptiInputValue
    , ptiInputProof
    } <-
    pmatch $
      pfromData $
        punsafeCoerce @(PAsData PTrieInsertRedeemer) (pto pscriptContext'redeemer)
  let validateInsertTrie =
        pinsert
          # pfromData ptiInputRoot
          # pfromData ptiInputKey
          # pfromData ptiInputValue
          # pfromData ptiInputProof
  pvalidateConditions
    [ pisRewardingScript (pdata pscriptContext'scriptInfo)
    , validateInsertTrie #== pfromData ptiOutputRoot
    ]
