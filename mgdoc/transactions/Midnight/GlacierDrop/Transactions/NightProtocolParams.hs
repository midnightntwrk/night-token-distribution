module Midnight.GlacierDrop.Transactions.NightProtocolParams where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Error (note)
import Data.Bifunctor (Bifunctor (first))
import GHC.Exts (fromList)
import Midnight.GlacierDrop.Contrib.Cardano.Api (AUTxO (..), foldToUTxO, toUTxO)
import Midnight.GlacierDrop.Contrib.ShowCBORHex (ShowCBORHex (..))
import Midnight.GlacierDrop.Scripts.ProtocolParams.Types (
  NightMintingParams (..),
 )
import Midnight.GlacierDrop.Scripts.ProtocolParams.Types qualified as S
import Midnight.GlacierDrop.Scripts.ProtocolParamsCompile (
  protocolParamsAssetName,
  versionedProtocolParamsScript,
 )
import Midnight.GlacierDrop.Transactions.Common (
  AddressScriptHashError,
  adjustForMinUTxO,
  getTxBody,
  makeHashableScriptData,
  makeScriptWitness,
  mkAddressInConwayEra,
  scriptHashFromReferenceInput,
  toPlutusTxOutRef,
  validationFailureScriptContexts,
  valueToTxOutValue,
 )

data InitNightProtocolParamsTxError era
  = InitNightProtocolParamsBalanceError (C.TxBodyErrorAutoBalance era)
  | InitNightProtocolParamsOtherError String

data InitNightProtocolParamsArgs era = InitNightProtocolParamsArgs
  { seedUTxO :: AUTxO era
  , systemStart :: C.SystemStart
  , eraHistory :: C.EraHistory
  , ledgerProtocolParams :: C.LedgerProtocolParameters era
  , nightProtocolParams :: NightMintingParams
  , networkId :: C.NetworkId
  }

data InitNightProtocolParamsError
  = InitNightTxBodyErrorAutoBalance (C.TxBodyErrorAutoBalance C.ConwayEra)
  | InitNightAddressScriptHashError AddressScriptHashError
  | InitNightOtherError String
  deriving (Show)

buildInitNightProtocolParamsTransaction
  :: InitNightProtocolParamsArgs C.ConwayEra
  -> Either InitNightProtocolParamsError (C.TxBody C.ConwayEra)
buildInitNightProtocolParamsTransaction args = do
  let C.TxOut changeAddress _ _ _ = args.seedUTxO.aTxOut
      seedTxOutRef = toPlutusTxOutRef args.seedUTxO.aTxIn
      nightProtocolParamsScriptSerialised = versionedProtocolParamsScript seedTxOutRef
      nightProtocolParamsScript = C.PlutusScript C.plutusScriptVersion nightProtocolParamsScriptSerialised
      nightProtocolParamsScriptHash = C.hashScript nightProtocolParamsScript
      nightProtocolParamsScriptAddress =
        C.makeShelleyAddress
          args.networkId
          (C.PaymentCredentialByScript nightProtocolParamsScriptHash)
          C.NoStakeAddress

      nightProtocolParamsToken = do
        let policyId = C.scriptPolicyId nightProtocolParamsScript
            assetName = protocolParamsAssetName
        C.AssetId policyId assetName

      txBodyContent =
        (C.defaultTxBodyContent C.ShelleyBasedEraConway)
          { C.txIns =
              [ (args.seedUTxO.aTxIn, C.BuildTxWith . C.KeyWitness $ C.KeyWitnessForSpending)
              ]
          , C.txInsCollateral =
              C.TxInsCollateral C.AlonzoEraOnwardsConway [args.seedUTxO.aTxIn]
          , C.txOuts =
              [ adjustForMinUTxO id args.ledgerProtocolParams $
                  C.TxOut
                    (mkAddressInConwayEra nightProtocolParamsScriptAddress)
                    (valueToTxOutValue . fromList $ [(nightProtocolParamsToken, C.Quantity 1)])
                    ( C.TxOutDatumInline C.BabbageEraOnwardsConway . makeHashableScriptData $
                        args.nightProtocolParams
                    )
                    C.ReferenceScriptNone
              ]
          , C.txProtocolParams = C.BuildTxWith (Just args.ledgerProtocolParams)
          , C.txMintValue =
              C.mkTxMintValue
                C.MaryEraOnwardsConway
                [
                  ( C.PolicyId nightProtocolParamsScriptHash
                  , C.PolicyAssets $ fromList [(protocolParamsAssetName, 1)]
                  , C.BuildTxWith $
                      C.PlutusScriptWitness
                        C.PlutusScriptV3InConway
                        C.PlutusScriptV3
                        (C.PScript nightProtocolParamsScriptSerialised)
                        C.NoScriptDatumForMint
                        (makeHashableScriptData seedTxOutRef)
                        (C.ExecutionUnits 0 0)
                  )
                ]
          }

  fmap getTxBody
    . first InitNightTxBodyErrorAutoBalance
    $ C.makeTransactionBodyAutoBalance
      C.ShelleyBasedEraConway
      args.systemStart
      (C.toLedgerEpochInfo args.eraHistory)
      args.ledgerProtocolParams
      mempty
      mempty
      mempty
      (toUTxO args.seedUTxO)
      txBodyContent
      changeAddress
      Nothing

data UpdateNightProtocolParamsArgs era = UpdateNightProtocolParamsArgs
  { eraHistory :: C.EraHistory
  , feeUTxO :: AUTxO C.ConwayEra
  , ledgerProtocolParams :: C.LedgerProtocolParameters era
  , networkId :: C.NetworkId
  , nightProtocolParams :: NightMintingParams
  , nightProtocolParamsScriptUTxO :: AUTxO era -- UTxO containing published Night ProtocolParams script
  , nightProtocolParamsUTxO :: AUTxO era -- Current protocol params UTxO
  , systemStart :: C.SystemStart
  }

data UpdateNightProtocolParamsError era
  = UpdateNightProtocolParamsTxBodyErrorAutoBalance String
  | UpdateNightProtocolParamsError String
  deriving (Show)

buildUpdateNightProtocolParamsTransaction
  :: UpdateNightProtocolParamsArgs C.ConwayEra
  -> Either (UpdateNightProtocolParamsError C.ConwayEra) (C.TxBody C.ConwayEra)
buildUpdateNightProtocolParamsTransaction args = do
  let UpdateNightProtocolParamsArgs
        { eraHistory
        , feeUTxO
        , ledgerProtocolParams
        , networkId = _networkId
        , nightProtocolParams
        , nightProtocolParamsScriptUTxO
        , nightProtocolParamsUTxO
        , systemStart
        } = args

      AUTxO feeTxIn (C.TxOut changeAddress _ _ _) = feeUTxO
      AUTxO
        nightProtocolParamsScriptTxIn
        nightProtocolParamsScriptTxOut@( C.TxOut
                                          nightProtocolParamsScriptAddress
                                          nightProtocolParamsScriptValue
                                          _nightProtocolParamsScriptDatum
                                          _
                                        ) = nightProtocolParamsScriptUTxO

      newDatum =
        C.TxOutDatumInline C.BabbageEraOnwardsConway $
          makeHashableScriptData nightProtocolParams

  nightProtocolParamsScriptHash <-
    note
      ( UpdateNightProtocolParamsError "Failed to get script hash from reference input"
      )
      $ scriptHashFromReferenceInput nightProtocolParamsScriptTxOut

  let txBodyContent =
        (C.defaultTxBodyContent C.ShelleyBasedEraConway)
          { C.txIns =
              [ (feeTxIn, C.BuildTxWith . C.KeyWitness $ C.KeyWitnessForSpending)
              ,
                ( nightProtocolParamsUTxO.aTxIn
                , makeScriptWitness
                    nightProtocolParamsScriptTxIn
                    nightProtocolParamsScriptHash
                    S.UpdateParameters
                )
              ]
          , C.txInsCollateral =
              C.TxInsCollateral C.AlonzoEraOnwardsConway [feeUTxO.aTxIn]
          , C.txInsReference =
              C.TxInsReference
                C.BabbageEraOnwardsConway
                [nightProtocolParamsScriptTxIn]
                (C.BuildTxWith mempty)
          , C.txOuts =
              [ adjustForMinUTxO id ledgerProtocolParams $
                  C.TxOut
                    nightProtocolParamsScriptAddress
                    nightProtocolParamsScriptValue
                    newDatum
                    C.ReferenceScriptNone
              ]
          , C.txProtocolParams = C.BuildTxWith (Just ledgerProtocolParams)
          }
      utxoSet =
        foldToUTxO
          [ nightProtocolParamsScriptUTxO
          , nightProtocolParamsUTxO
          , feeUTxO
          ]
  fmap getTxBody
    . first
      ( \err ->
          UpdateNightProtocolParamsTxBodyErrorAutoBalance $
            show err
              <> "\n"
              <> show
                ( ShowCBORHex
                    <$> validationFailureScriptContexts
                      C.ShelleyBasedEraConway
                      systemStart
                      (C.toLedgerEpochInfo eraHistory)
                      ledgerProtocolParams
                      utxoSet
                      txBodyContent
                      changeAddress
                )
      )
    $ C.makeTransactionBodyAutoBalance
      C.ShelleyBasedEraConway
      systemStart
      (C.toLedgerEpochInfo eraHistory)
      ledgerProtocolParams
      mempty
      mempty
      mempty
      utxoSet
      txBodyContent
      changeAddress
      Nothing
