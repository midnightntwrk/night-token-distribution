{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Midnight.GlacierDrop.Contrib.Cardano.InMemory (
  CardanoInMem,
  LedgerInfo (..),
  ExecResult,
  execInMemUsingNodeLedgerInfo,
  execCardanoInMem,
  getUTxOByTxIn,
  logMsg,
  queryUTxOs,
  submitTxBody,
  queryLedgerInfo,
)
where

import Cardano.Api (MonadError (throwError))
import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as CLedger
import Cardano.Api.Network qualified as C
import Cardano.Api.Shelley qualified as CShelley
import Cardano.Ledger.Alonzo.Core (
  ppMaxTxExUnitsL,
  ppMaxTxSizeL,
 )
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.TxWits qualified as C
import Control.Error (hush)
import Control.Lens ((^.))
import Control.Monad (join, unless)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get, modify')
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.RWS (RWS)
import Control.Monad.Trans.RWS qualified as RWS
import Control.Monad.Writer.Class (tell)
import Data.ByteString qualified as BS
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC.Generics (Generic)
import GHC.Natural (Natural, naturalFromInteger)
import Lens.Micro qualified as Micro
import Midnight.GlacierDrop.Contrib.Cardano.Api (AUTxO (..))
import Midnight.GlacierDrop.Contrib.Data.Foldable (foldMapMFlipped)

-- Let's specialize era to `ConwayEra` - we can generalize later when needed
type UTxOSet = C.UTxO C.ConwayEra
type SubmittedTransactions = [C.TxBody C.ConwayEra]
type Logs = [String]
data LedgerInfo = LedgerInfo
  { eraHistory :: C.EraHistory
  , networkId :: C.NetworkId
  , protocolParameters :: CShelley.LedgerProtocolParameters C.ConwayEra
  , systemStart :: C.SystemStart
  }

newtype CardanoInMem a = CardanoInMem
  { runCardanoInMem
      :: ExceptT String (RWS LedgerInfo (SubmittedTransactions, Logs) UTxOSet) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadError String)

type ExecResult a = (Either String a, UTxOSet, (SubmittedTransactions, Logs))

-- The joy of hushing
hushHushJoin :: Either e (Either e' a) -> Maybe a
hushHushJoin eea = do
  ea <- hush eea
  hush ea

hushJoin :: Either e (Maybe a) -> Maybe a
hushJoin = join . hush

logMsg :: String -> CardanoInMem ()
logMsg s = CardanoInMem $ tell ([], [s])

-- | Use the local cardano node to:
-- | * initialize the ledger info
-- | * prepopulate the UTxO set
-- | Then run the action in the in-memory environment.
-- | The outer `Either` is for the initialization failure.
execInMemUsingNodeLedgerInfo
  :: C.LocalNodeConnectInfo
  -> [C.QueryUTxOFilter]
  -> CardanoInMem a
  -> IO (ExecResult a)
execInMemUsingNodeLedgerInfo connectInfo queries m = do
  initializationResult <- fmap hushJoin . C.executeLocalStateQueryExpr connectInfo C.VolatileTip $ runMaybeT do
    eraHistory <- MaybeT $ C.queryEraHistory <&> hush
    protocolParams <-
      MaybeT $ C.queryProtocolParameters C.ShelleyBasedEraConway <&> hushHushJoin
    systemStart <- MaybeT $ C.querySystemStart <&> hush
    utxo <-
      C.UTxO <$> foldMapMFlipped queries \query -> do
        C.UTxO utxoMap <-
          MaybeT $ C.queryUtxo C.ShelleyBasedEraConway query <&> hushHushJoin
        pure utxoMap
    let C.LocalNodeConnectInfo _ networkId _ = connectInfo
    pure
      ( LedgerInfo
          eraHistory
          networkId
          (CShelley.LedgerProtocolParameters protocolParams)
          systemStart
      , utxo
      )
  case initializationResult of
    Just (ledgerInfo, utxo) -> pure $ execCardanoInMem m ledgerInfo utxo
    Nothing -> pure (Left "Initialization failed", C.UTxO mempty, ([], []))

-- | Run the action in the in-memory environment.
execCardanoInMem
  :: CardanoInMem a
  -> LedgerInfo
  -> UTxOSet
  -> (Either String a, UTxOSet, (SubmittedTransactions, Logs))
execCardanoInMem (CardanoInMem action) ledgerInfo utxo = do
  let (result, utxos, (txs, logs)) = RWS.runRWS (runExceptT action) ledgerInfo utxo
  (result, utxos, (txs, logs))

type UTxO = C.UTxO C.ConwayEra

utxoToList :: forall era. C.UTxO era -> [(C.TxIn, C.TxOut C.CtxUTxO era)]
utxoToList = Map.toList . C.unUTxO

utxoFromList :: forall era. [(C.TxIn, C.TxOut C.CtxUTxO era)] -> C.UTxO era
utxoFromList = C.UTxO . Map.fromList

toAddressAny' :: C.AddressInEra era -> C.AddressAny
toAddressAny' (C.AddressInEra _ addr) = C.toAddressAny addr

newtype Percent = Percent {unPercent :: Natural}
  deriving stock (Eq, Generic, Show)

data TxResourceUsage = TxResourceUsage
  { elMemory :: (Natural, Percent)
  , elSteps :: (Natural, Percent)
  , elSize :: (Natural, Percent)
  }
  deriving stock (Eq, Show, Generic)

newtype ExecutionLimitsExceeded = ExecutionLimitsExceeded {unExecutionLimitsExceeded :: TxResourceUsage}
  deriving stock (Eq, Show, Generic)

exUnits
  :: C.TxBody era
  -> ExUnits
exUnits
  ( C.ShelleyTxBody
      C.ShelleyBasedEraBabbage
      _
      _
      (C.TxBodyScriptData _ _ (C.Redeemers redeemers))
      _
      _
    ) =
    mconcat . fmap snd . Map.elems $ redeemers
exUnits
  ( C.ShelleyTxBody
      C.ShelleyBasedEraAlonzo
      _
      _
      (C.TxBodyScriptData _ _ (C.Redeemers redeemers))
      _
      _
    ) =
    mconcat . fmap snd . Map.elems $ redeemers
exUnits _ = mempty

txResourceUsage
  :: CShelley.LedgerProtocolParameters C.ConwayEra
  -> C.TxBody C.ConwayEra
  -> TxResourceUsage
txResourceUsage protocolParameters txBody =
  let lppPParamsL
        :: Micro.Getting
            f
            (CShelley.LedgerProtocolParameters era)
            (CLedger.PParams (CShelley.ShelleyLedgerEra era))
      lppPParamsL = Micro.to \(CShelley.LedgerProtocolParameters pp) -> pp
      size =
        naturalFromInteger
          $ toInteger
          $ BS.length
          $ C.shelleyBasedEraConstraints
            (C.babbageEraOnwardsToShelleyBasedEra C.BabbageEraOnwardsConway)
          $ C.serialiseToCBOR txBody
      maxSize =
        C.babbageEraOnwardsConstraints C.BabbageEraOnwardsConway $
          fromIntegral $
            protocolParameters ^. lppPParamsL . ppMaxTxSizeL
      fractionSize = 100 * size `div` maxSize
      ExUnits memory steps = exUnits txBody
      maxExecutionUnits =
        C.babbageEraOnwardsConstraints C.BabbageEraOnwardsConway $
          Just . CShelley.fromAlonzoExUnits $
            protocolParameters ^. lppPParamsL . ppMaxTxExUnitsL
      fractionMemory = 100 * memory `div` maybe 0 C.executionMemory maxExecutionUnits
      fractionSteps = 100 * steps `div` maybe 0 C.executionSteps maxExecutionUnits
   in TxResourceUsage
        { elMemory = (memory, Percent fractionMemory)
        , elSteps = (steps, Percent fractionSteps)
        , elSize = (size, Percent fractionSize)
        }

checkTxLimits
  :: C.TxBody C.ConwayEra
  -- ^ The transaction body.
  -> CardanoInMem (Maybe ExecutionLimitsExceeded)
  -- ^ The error if the transaction exceeds the limits.
checkTxLimits txBody = CardanoInMem do
  pp <- ask <&> protocolParameters
  let -- we are not able to define this helper in the same `let` block below :-)
      unPercent' = unPercent . snd
  let usage = txResourceUsage pp txBody
      TxResourceUsage
        { elMemory = unPercent' -> elMemoryPercent
        , elSteps = unPercent' -> elStepsPercent
        , elSize = unPercent' -> elSizePercent
        } = usage
  pure $
    if elMemoryPercent >= 100 || elStepsPercent >= 100 || elSizePercent >= 100
      then Just $ ExecutionLimitsExceeded usage
      else Nothing

queryUTxOs
  :: C.QueryUTxOFilter
  -> CardanoInMem UTxO
queryUTxOs queryFilterFilter = CardanoInMem do
  utxos <- get
  case queryFilterFilter of
    C.QueryUTxOByAddress addresses -> do
      let utxosList = utxoToList utxos
          filterStep (_, C.TxOut address _ _ _) =
            Set.member (toAddressAny' address) addresses
      pure $ utxoFromList $ filter filterStep utxosList
    C.QueryUTxOWhole -> pure utxos
    C.QueryUTxOByTxIn txins -> do
      let utxosList = utxoToList utxos
          filterStep (txin, _) = Set.member txin txins
      pure $ utxoFromList $ filter filterStep utxosList

getUTxOByTxIn :: C.TxIn -> CardanoInMem (AUTxO C.ConwayEra)
getUTxOByTxIn txIn = do
  utxos <- queryUTxOs (C.QueryUTxOByTxIn $ Set.singleton txIn)
  case utxoToList utxos of
    [(_, txOut)] -> pure (AUTxO txIn txOut)
    _ -> throwError $ "UTxO not found for TxIn: " <> show txIn

submitTxBody
  :: C.TxBody C.ConwayEra
  -> CardanoInMem C.TxId
submitTxBody txBody = CardanoInMem do
  let txId = C.getTxId txBody
  let C.TxBody txBodyContent = txBody
      txIns = map fst . C.txIns $ txBodyContent
      txInsRef = case C.txInsReference txBodyContent of
        C.TxInsReferenceNone -> []
        C.TxInsReference _ ins _ -> ins
      txOuts = map C.toCtxUTxOTxOut . C.txOuts $ txBodyContent
      newUTxOs = zip [0 ..] txOuts <&> \(txIx, txOut) -> (C.TxIn txId (C.TxIx txIx), txOut)

  utxosMap <- get <&> C.unUTxO
  let missingTxIns = filter (`Map.notMember` utxosMap) (txIns ++ txInsRef)
  unless (null missingTxIns) $
    throwError $
      "UTxOs missing:"
        <> show missingTxIns
        <> " . All UTxOs: "
        <> show (Map.keys utxosMap)

  -- Validate execution budget
  runCardanoInMem (checkTxLimits txBody) >>= traverse_ (throwError . show)

  tell ([txBody], [])

  modify' \utxos -> do
    let utxosList = utxoToList utxos
        utxosList' =
          newUTxOs
            <> filter (\(txIn, _) -> txIn `notElem` txIns) utxosList
        utxos' = utxoFromList utxosList'
    utxos'
  pure txId

queryLedgerInfo :: CardanoInMem LedgerInfo
queryLedgerInfo = CardanoInMem ask
