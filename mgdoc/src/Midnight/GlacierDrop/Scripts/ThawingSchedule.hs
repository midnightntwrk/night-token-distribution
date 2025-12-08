module Midnight.GlacierDrop.Scripts.ThawingSchedule where

import GHC.Generics (Generic)
import Midnight.GlacierDrop.Scripts.Common (
  ContBool,
  diffTime,
  trace,
  traceError,
 )
import PlutusLedgerApi.V1.Time (DiffMilliSeconds (..))
import PlutusLedgerApi.V3 as P
import PlutusPrelude (coerce)
import PlutusTx.List (replicate, zip)
import PlutusTx.Prelude hiding (Eq, error, trace, traceError)
import PlutusTx.Prelude qualified as P
import Prelude qualified as H

newtype GenesisTimestamp = GenesisTimestamp {getGenesisTimestamp :: POSIXTime}
  deriving (H.Show)
  deriving newtype (H.Eq, P.UnsafeFromData, P.FromData, P.ToData, P.Eq, P.Ord)

newtype RedemptionIncrementPeriod = RedemptionIncrementPeriod {getRedemptionIncrementPeriod :: DiffMilliSeconds}
  deriving (H.Show, Generic)
  deriving newtype (H.Eq, P.UnsafeFromData, P.FromData, P.ToData, P.Eq, P.Ord)

-- | The number of segments into which we divide a single redemption period.
newtype JitterStrataCount = JitterStrataCount {getJitterStrataCount :: Integer}
  deriving (H.Show)
  deriving newtype (H.Eq, P.UnsafeFromData, P.FromData, P.ToData, P.Eq, P.Ord)

-- | A particular segment of the redemption period which was chosen for a particular claimant.
-- | It is a number between 0 and `jitterStrataCount - 1` and because it is derived pseudo-randomly
-- | (using signature bytes) it should distribute claimants evenly across the period.
newtype JitterStratum = JitterStratum {getJitterStratum :: Integer}
  deriving (H.Show)
  deriving newtype (H.Eq, P.UnsafeFromData, P.FromData, P.ToData, P.Eq, P.Ord)

-- | We could called it `Now` but it would be more confusing in the context of tx validation.
-- | This represents a point in time which for sure is in the past.
newtype PastPoint = PastPoint POSIXTime
  deriving (H.Show)
  deriving newtype (H.Eq, P.UnsafeFromData, P.FromData, P.ToData, P.Eq, P.Ord)

newtype ThawTime = ThawTime POSIXTime
  deriving (H.Show)
  deriving newtype (H.Eq, P.UnsafeFromData, P.FromData, P.ToData, P.Eq, P.Ord)

newtype StarAmount = StarAmount {getStarAmount :: Integer}
  deriving (H.Show)
  deriving newtype (H.Eq, P.UnsafeFromData, P.FromData, P.ToData, P.Eq, P.Ord)

newtype IncrementsLeft = IncrementsLeft Integer
  deriving (H.Show)
  deriving newtype (H.Eq, P.UnsafeFromData, P.FromData, P.ToData, P.Eq, P.Ord)

{-# INLINEABLE pastPointFromValidityInterval #-}
pastPointFromValidityInterval :: POSIXTimeRange -> ContBool PastPoint
pastPointFromValidityInterval (Interval l _) continue = case l of
  LowerBound NegInf _ -> trace "pastPointFromValidityInterval NegInf" False
  LowerBound (Finite t) closure -> continue $ coerce (t + if closure then 0 else 1)
  LowerBound PosInf _ -> trace "pastPointFromValidityInterval PosInf" False

{-# INLINEABLE finiteTxValidityRange #-}
finiteTxValidityRange :: POSIXTimeRange -> (Integer, Integer)
finiteTxValidityRange (Interval l u) = case (l, u) of
  (LowerBound (Finite fromT) closureStart, UpperBound (Finite toT) closureEnd) ->
    ( coerce fromT + if closureStart then 0 else 1
    , coerce toT + if closureEnd then 0 else 1
    )
  _ ->
    traceError "finiteTxValidityRange: either lower or upper bound is not finite"

{-# INLINEABLE shiftTimestamp #-}
shiftTimestamp :: POSIXTime -> DiffMilliSeconds -> POSIXTime
shiftTimestamp (POSIXTime t) (DiffMilliSeconds dt) = coerce (t + dt)

{-# INLINEABLE initialThaw #-}
initialThaw
  :: GenesisTimestamp
  -> RedemptionIncrementPeriod
  -> JitterStratum
  -> JitterStrataCount
  -> ThawTime
initialThaw
  (GenesisTimestamp genesisTimestamp)
  (RedemptionIncrementPeriod (DiffMilliSeconds incrementPeriod))
  (JitterStratum jitterStratum)
  (JitterStrataCount jitterStrataCount) = do
    let -- Jitter is just a fraction of the period.
        -- Parentheses seem to decrease readability in this case but it is better to have them.
        jitter :: DiffMilliSeconds
        jitter = coerce ((incrementPeriod * jitterStratum) `divide` jitterStrataCount)
    coerce (shiftTimestamp genesisTimestamp jitter)

data ThawingScheduleReduction
  = ThawingScheduleNotReduced
  | ThawingSchedulePartiallyReduced StarAmount ThawTime IncrementsLeft
  | ThawingScheduleFullyReduced
  deriving (H.Show)

{-# INLINEABLE reduceThawingSchedule #-}
reduceThawingSchedule
  :: RedemptionIncrementPeriod
  -> PastPoint
  -> StarAmount
  -- ^ amount received per increment
  -> ThawTime
  -> IncrementsLeft
  -> ThawingScheduleReduction
reduceThawingSchedule
  (RedemptionIncrementPeriod (DiffMilliSeconds incrementPeriod))
  (PastPoint now)
  (StarAmount incrementAmountQuotient)
  (ThawTime previousIncrement)
  (IncrementsLeft incrementsLeft)
    | thawedIncrements == incrementsLeft = ThawingScheduleFullyReduced
    | thawedIncrements <= 0 = ThawingScheduleNotReduced
    | otherwise = do
        let amountThawed =
              -- All the thaws should be nearly equal - they are rounded down.
              -- The last thawing can be a little bit bigger because it includes the remainder.
              -- Parentheses seem to decrease readability in this case but it is better to have them.
              coerce $ incrementAmountQuotient * thawedIncrements
            nextThaw = coerce shiftTimestamp previousIncrement (incrementPeriod * thawedIncrements)
        ThawingSchedulePartiallyReduced
          amountThawed
          nextThaw
          (coerce (incrementsLeft - thawedIncrements))
    where
      thawedIncrements :: Integer
      thawedIncrements = min incrementsLeft do
        let DiffMilliSeconds fromFirstIncrement = now `diffTime` previousIncrement
        if fromFirstIncrement < 0
          then 0
          else (fromFirstIncrement `divide` incrementPeriod) + 1

{-# INLINEABLE incrementalSchedule #-}
incrementalSchedule
  :: StarAmount
  -> IncrementsLeft
  -> GenesisTimestamp
  -> RedemptionIncrementPeriod
  -> JitterStratum
  -> JitterStrataCount
  -> [(ThawTime, StarAmount)]
incrementalSchedule
  (StarAmount totalAmount)
  (IncrementsLeft totalIncrements)
  genesisTimestamp
  redemptionIncrementPeriod@(RedemptionIncrementPeriod (DiffMilliSeconds incrementPeriod))
  jitterStratum
  jitterStrataCount =
    coerce @[(Integer, Integer)]
      $ zip
        ( (\n -> initialTime + n * incrementPeriod) <$> enumFromTo 0 (totalIncrements - 2)
        )
        (replicate (totalIncrements - 1) incrementAmountQuotient)
      <> [
           ( initialTime + (totalIncrements - 1) * incrementPeriod
           , totalAmount - (totalIncrements - 1) * incrementAmountQuotient
           )
         ]
    where
      initialTime =
        coerce
          $ initialThaw
            genesisTimestamp
            redemptionIncrementPeriod
            jitterStratum
            jitterStrataCount
      incrementAmountQuotient = totalAmount `divide` totalIncrements
