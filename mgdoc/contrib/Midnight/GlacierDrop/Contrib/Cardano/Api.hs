{-# OPTIONS_GHC -Wno-orphans #-}

module Midnight.GlacierDrop.Contrib.Cardano.Api where

import Cardano.Api as C
import Cardano.Api.Shelley as C
import Cardano.Binary (FromCBOR (..), enforceSize, toCBOR)
import qualified Data.List as List
import Data.Map.Strict as M
import Data.Typeable (Typeable)

instance (Typeable ctx) => FromCBOR (TxOut ctx C.ConwayEra) where
  fromCBOR = C.fromShelleyTxOut C.ShelleyBasedEraConway <$> fromCBOR

instance ToCBOR (TxOut C.CtxUTxO C.ConwayEra) where
  toCBOR = toCBOR . C.toShelleyTxOut C.ShelleyBasedEraConway

instance FromCBOR TxIn where
  fromCBOR = do
    enforceSize "TxIn" 2
    txId <- fromCBOR
    txIx <- fromCBOR
    case C.deserialiseFromRawBytes C.AsTxId txId of
      Right txId' -> pure $ C.TxIn txId' (C.TxIx txIx)
      Left _ -> fail "Couldn't parse txin"

instance ToCBOR TxIn where
  toCBOR (C.TxIn txId (TxIx txIx)) = do
    let txIdBytes = C.serialiseToRawBytes txId
    toCBOR (txIdBytes, txIx)

-- Trivial helper which combines the full info about unspent output
data AUTxO era = AUTxO {aTxIn :: C.TxIn, aTxOut :: C.TxOut C.CtxUTxO era}
  deriving (Show)

instance FromCBOR (AUTxO C.ConwayEra) where
  fromCBOR = do
    enforceSize "AUTxO" 2
    AUTxO <$> fromCBOR <*> fromCBOR

instance ToCBOR (AUTxO C.ConwayEra) where
  toCBOR (AUTxO txin txout) =
    toCBOR txin
      <> toCBOR txout

toUTxO :: AUTxO era -> C.UTxO era
toUTxO (AUTxO txin txout) = C.UTxO . M.fromList $ [(txin, txout)]

foldToUTxO :: (Foldable f) => f (AUTxO era) -> C.UTxO era
foldToUTxO =
  C.UTxO
    . foldMap (fromList . List.singleton . \(AUTxO txin txout) -> (txin, txout))
