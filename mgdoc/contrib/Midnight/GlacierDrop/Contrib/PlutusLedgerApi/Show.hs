{-# OPTIONS_GHC -Wno-orphans #-}

module Midnight.GlacierDrop.Contrib.PlutusLedgerApi.Show where

import Data.ByteString (ByteString)
import GHC.Show (appPrec, appPrec1)
import Generic.Data (Generically (Generically))
import Midnight.GlacierDrop.Contrib.Generic.Data.Internal.Show (
  Show (showList, showsPrec),
  shows,
 )
import PlutusLedgerApi.V3 (
  Address,
  BuiltinData,
  ChangedParameters,
  ColdCommitteeCredential,
  Constitution,
  Credential,
  CurrencySymbol,
  DRep,
  DRepCredential,
  Data,
  Datum,
  DatumHash,
  Delegatee,
  Extended,
  GovernanceAction,
  GovernanceActionId,
  HotCommitteeCredential,
  Interval,
  Lovelace,
  LowerBound,
  MintValue,
  OutputDatum,
  POSIXTime,
  ProposalProcedure,
  ProtocolVersion,
  PubKeyHash,
  Rational,
  Redeemer,
  ScriptContext,
  ScriptHash,
  ScriptInfo,
  ScriptPurpose,
  StakingCredential,
  TokenName (unTokenName),
  TxCert,
  TxId,
  TxInInfo,
  TxInfo,
  TxOut,
  TxOutRef,
  UpperBound,
  Value,
  Vote,
  Voter,
 )
import PlutusTx.AssocMap (Map)
import PlutusTx.AssocMap qualified as M
import Prelude hiding (Rational, Show (..), shows)
import Prelude qualified as P

deriving via (Generically ScriptContext) instance Show ScriptContext
deriving via (Generically TxInfo) instance Show TxInfo
instance (Show k, Show v) => Show (Map k v) where
  showsPrec p m =
    showParen (p > appPrec) $
      showString "unsafeFromList " . showsPrec appPrec1 (M.toList m)
instance (Show a) => Show [a] where showsPrec _ = showList
instance (Show a, Show b) => Show (a, b) where
  showsPrec _ (a, b) = showChar '(' . shows a . showChar ',' . shows b . showChar ')'
deriving via (Generically (Interval a)) instance (Show a) => Show (Interval a)
deriving via
  (Generically (LowerBound a))
  instance
    (Show a) => Show (LowerBound a)
deriving via (Generically (Extended a)) instance (Show a) => Show (Extended a)
deriving via (Generically Bool) instance Show Bool
deriving via
  (Generically (UpperBound a))
  instance
    (Show a) => Show (UpperBound a)
deriving via (Generically (Maybe a)) instance (Show a) => Show (Maybe a)
deriving via (Generically TxInInfo) instance Show TxInInfo
deriving via (Generically TxOutRef) instance Show TxOutRef
instance Show TxId where showsPrec p = betweenQuotes . P.showsPrec p
instance Show Integer where showsPrec = P.showsPrec
deriving via (Generically MintValue) instance Show MintValue
deriving via (Generically TxOut) instance Show TxOut
deriving via (Generically Address) instance Show Address
deriving via (Generically Credential) instance Show Credential
instance Show PubKeyHash where showsPrec p = betweenQuotes . P.showsPrec p
instance Show ScriptHash where showsPrec p = betweenQuotes . P.showsPrec p
deriving via (Generically StakingCredential) instance Show StakingCredential
deriving via (Generically Value) instance Show Value
instance Show CurrencySymbol where showsPrec p = betweenQuotes . P.showsPrec p
instance Show TokenName where showsPrec p = P.showsPrec p . unTokenName
deriving via (Generically OutputDatum) instance Show OutputDatum
instance Show DatumHash where showsPrec = P.showsPrec
deriving via (Generically Datum) instance Show Datum
deriving via (Generically BuiltinData) instance Show BuiltinData
deriving via (Generically Data) instance Show Data
instance Show ByteString where showsPrec = P.showsPrec
deriving via (Generically Lovelace) instance Show Lovelace
deriving via (Generically TxCert) instance Show TxCert
deriving via (Generically Delegatee) instance Show Delegatee
deriving via (Generically DRep) instance Show DRep
deriving via (Generically DRepCredential) instance Show DRepCredential
deriving via
  (Generically ColdCommitteeCredential)
  instance
    Show ColdCommitteeCredential
deriving via
  (Generically HotCommitteeCredential)
  instance
    Show HotCommitteeCredential
deriving via (Generically POSIXTime) instance Show POSIXTime
deriving via (Generically ScriptPurpose) instance Show ScriptPurpose
deriving via (Generically Voter) instance Show Voter
deriving via (Generically ProposalProcedure) instance Show ProposalProcedure
deriving via (Generically GovernanceAction) instance Show GovernanceAction
deriving via (Generically GovernanceActionId) instance Show GovernanceActionId
deriving via (Generically ChangedParameters) instance Show ChangedParameters
deriving via (Generically ProtocolVersion) instance Show ProtocolVersion
deriving via (Generically Rational) instance Show Rational
deriving via (Generically Constitution) instance Show Constitution
deriving via (Generically Redeemer) instance Show Redeemer
deriving via (Generically Vote) instance Show Vote
deriving via (Generically ScriptInfo) instance Show ScriptInfo

betweenQuotes :: ShowS -> ShowS
betweenQuotes a = showChar '"' . a . showChar '"'
