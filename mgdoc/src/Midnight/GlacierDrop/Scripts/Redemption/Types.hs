{-# LANGUAGE TemplateHaskell #-}

module Midnight.GlacierDrop.Scripts.Redemption.Types where

import Data.Eq (Eq)
import GHC.Generics (Generic)
import Midnight.GlacierDrop.Scripts.ThawingSchedule (
  RedemptionIncrementPeriod (RedemptionIncrementPeriod),
 )
import PlutusLedgerApi.V1.Time (DiffMilliSeconds (DiffMilliSeconds))
import PlutusLedgerApi.V3 (
  Address (Address),
  Credential (PubKeyCredential),
  POSIXTime (POSIXTime),
 )
import PlutusTx (makeIsDataIndexed)
import PlutusTx.Foldable qualified as P
import PlutusTx.Prelude hiding (Eq, error)
import PlutusTx.Show qualified as P
import Text.Show (Show)

data RedemptionDatum = RedemptionDatum
  { destinationAddress :: Address
  -- ^ The address allowed to receive the thawed STAR tokens.
  , incrementAmountQuotient :: Integer
  -- ^ The star amount received per increment.
  , nextThaw :: POSIXTime
  -- ^ The instant of the next thaw increment.
  , incrementsLeft :: Integer
  -- ^ The number of increments remaining in the schedule.
  , redemptionIncrementPeriod :: RedemptionIncrementPeriod
  -- ^ The duration of the period between thaw increments.
  }
  deriving (Eq, Show, Generic)

instance P.Show RedemptionDatum where
  {-# INLINEABLE show #-}
  show
    ( RedemptionDatum
        (Address (PubKeyCredential destinationAddress) _)
        incrementAmountQuotient
        (POSIXTime nextThaw)
        incrementsLeft
        (RedemptionIncrementPeriod (DiffMilliSeconds redemptionIncrementPeriod))
      ) =
      P.fold
        [ "RedemptionDatum"
        , "{ destinationAddress = "
        , P.show destinationAddress
        , ", incrementAmountQuotient = "
        , P.show incrementAmountQuotient
        , ", nextThaw = "
        , P.show nextThaw
        , ", incrementsLeft = "
        , P.show incrementsLeft
        , ", redemptionIncrementPeriod = "
        , P.show redemptionIncrementPeriod
        , "}"
        ]
  show _ = "Invalid RedemptionDatum"

makeIsDataIndexed
  ''RedemptionDatum
  [('RedemptionDatum, 0)]

data RedemptionRedeemer
  = Thaw
  deriving (Eq, Show, Generic)

makeIsDataIndexed
  ''RedemptionRedeemer
  [('Thaw, 0)]
