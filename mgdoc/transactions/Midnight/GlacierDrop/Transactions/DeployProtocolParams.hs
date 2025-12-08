{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Midnight.GlacierDrop.Transactions.DeployProtocolParams where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Shelley.TxCert qualified as TxCert
import Control.Monad (forM_, when)
import Control.Monad.Except (MonadError)
import Convex.BuildTx
import Convex.Class (
  MonadBlockchain (queryNetworkId),
  MonadUtxoQuery (..),
  queryProtocolParameters,
  utxosByPaymentCredential,
 )
import Convex.CoinSelection qualified
import Convex.CoinSelection qualified as CoinSelection
import Convex.PlutusLedger.V1 qualified as PL
import Convex.PlutusLedger.V3 qualified as PL3
import Convex.Utxos qualified as Utxos
import Convex.Wallet.Operator (returnOutputFor)
import Data.Bifunctor (Bifunctor (first))
import Data.Either (fromRight)
import Data.Map qualified as M
import GHC.Exts (fromList)
import Midnight.GlacierDrop.Scripts.ProtocolParams.Types (
  NightMintingParams (..),
  UpgradeParamsAction (..),
 )
import Midnight.GlacierDrop.Scripts.ProtocolParamsCompile (
  protocolParamsAssetName,
  versionedProtocolParamsScript,
 )
import Midnight.GlacierDrop.Transactions.Common (
  deserializeDatumInline,
  getTxBody,
 )

data DeployProtocolParamsArgs era = DeployProtocolParamsArgs
  { deployFeeTxIn :: C.TxIn
  , deploySystemStart :: C.SystemStart
  , deployEraHistory :: C.EraHistory
  , deployLedgerProtocolParams :: C.LedgerProtocolParameters era
  , deployTxIn :: C.TxIn
  , deployFeeInput :: C.TxOut C.CtxUTxO era
  , deployNightProtocolParams :: NightMintingParams
  , deployNetworkId :: C.NetworkId
  , deployWallet :: C.Address C.ShelleyAddr
  }

data UpdateProtocolParamsArgs era = UpdateProtocolParamsArgs
  { ppTxIn :: C.TxIn
  , ppInput :: C.TxOut C.CtxUTxO era
  , newNightProtocolParams :: NightMintingParams
  , feeTxIn :: C.TxIn
  , protocolParamsScript :: C.PlutusScript C.PlutusScriptV3
  , networkId :: C.NetworkId
  , signers :: [C.Hash C.PaymentKey]
  , systemStart :: C.SystemStart
  , eraHistory :: C.EraHistory
  , protocolParams :: C.LedgerProtocolParameters era
  }

initProtocolParams
  :: ( MonadBuildTx era m
     , C.IsBabbageBasedEra era
     , C.HasScriptLanguageInEra C.PlutusScriptV3 era
     )
  => C.NetworkId
  -> C.TxIn
  -> NightMintingParams
  -> m ()
initProtocolParams networkId ppInitTxOutRef nightProtocolParams = do
  spendPublicKeyOutput ppInitTxOutRef
  let mintRedeemer = ()
  let ppScript = versionedProtocolParamsScript $ PL3.transTxOutRef ppInitTxOutRef
  -- Mint the night protocol params NFT
  mintPlutus ppScript mintRedeemer protocolParamsAssetName 1
  -- Add the night protocol params output with the NFT
  addProtocolParamsOutput networkId ppScript nightProtocolParams
  pure ()

addProtocolParamsOutput
  :: (MonadBuildTx era m, C.IsBabbageBasedEra era)
  => C.NetworkId
  -> C.PlutusScript C.PlutusScriptV3
  -> NightMintingParams
  -> m ()
addProtocolParamsOutput nid djProtocolParamScript ppDatum =
  payToScriptInlineDatum
    nid
    (C.hashScript $ C.PlutusScript C.plutusScriptVersion djProtocolParamScript)
    ppDatum
    C.NoStakeAddress
    (protocolParamsNFTValue djProtocolParamScript (C.Quantity 1))

protocolParamsNFTValue
  :: C.PlutusScript C.PlutusScriptV3 -> C.Quantity -> C.Value
protocolParamsNFTValue ppScript n =
  let policyId = C.scriptPolicyId $ C.PlutusScript C.plutusScriptVersion ppScript
      assetName = protocolParamsAssetName
   in fromList [(C.AssetId policyId assetName, n)]

updateProtocolParams
  :: ( MonadBuildTx era m
     , C.IsBabbageBasedEra era
     , C.HasScriptLanguageInEra C.PlutusScriptV3 era
     )
  => C.NetworkId
  -> C.TxIn
  -> C.PlutusScript C.PlutusScriptV3
  -> C.TxOut C.CtxUTxO era
  -> NightMintingParams
  -> [C.Hash C.PaymentKey]
  -> m ()
updateProtocolParams networkId ppTxIn ppScript protocolParamsInput newNightProtocolParams signers = do
  let redeemer = UpdateParameters

  -- Spend the night protocol params NFT with the `UpdateParameters` redeemer.
  spendPlutusInlineDatum ppTxIn ppScript redeemer

  -- Produce the night protocol params continuing output.
  addProtocolParamsOutput networkId ppScript newNightProtocolParams

  let NightMintingParams{tgeAgentAuthKeys, minAuthSignatures} =
        fromRight (error "updateProtocolParams: invalid night protocol params") $
          deserializeDatumInline @NightMintingParams protocolParamsDatum
  let authKeys = map (either (error . show) id . PL.unTransPubKeyHash) tgeAgentAuthKeys

  -- check that required signatories threshold is met
  when
    (length (filter (`elem` authKeys) signers) < fromIntegral minAuthSignatures)
    $ error "updateProtocolParams: required signatories threshold not met"

  -- add the required signers to the transaction
  forM_ signers $ \signer -> addRequiredSignature signer
  where
    C.TxOut _protocolParamsAddress _protocolParamsValue protocolParamsDatum _ = protocolParamsInput

data DeployProtocolParamsTxError era
  = DeployProtocolParamsBalanceError (C.TxBodyErrorAutoBalance era)
  | DeployProtocolParamsOtherError String

deployProtocolParamsUnbalanced
  :: forall era m
   . ( MonadBuildTx era m
     , C.IsBabbageBasedEra era
     , C.HasScriptLanguageInEra C.PlutusScriptV3 era
     )
  => DeployProtocolParamsArgs era
  -> m ()
deployProtocolParamsUnbalanced args = do
  initProtocolParams
    (deployNetworkId args)
    (deployTxIn args)
    (deployNightProtocolParams args)
  addCollateral (deployFeeTxIn args)

deployProtocolParamsTx
  :: forall era
   . (C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era)
  => DeployProtocolParamsArgs era
  -> Either (DeployProtocolParamsTxError era) (C.TxBody era)
deployProtocolParamsTx args = C.babbageEraOnwardsConstraints @era C.babbageBasedEra $ do
  let tx = buildTx @era $ execBuildTx @era $ deployProtocolParamsUnbalanced args
  fmap getTxBody
    . first DeployProtocolParamsBalanceError
    $ C.makeTransactionBodyAutoBalance
      (C.shelleyBasedEra @era)
      (deploySystemStart args)
      (C.toLedgerEpochInfo $ deployEraHistory args)
      (deployLedgerProtocolParams args)
      mempty
      mempty
      mempty
      (C.UTxO $ M.fromList [(deployFeeTxIn args, deployFeeInput args)])
      tx
      ( C.AddressInEra
          (C.ShelleyAddressInEra (C.shelleyBasedEra @era))
          (deployWallet args)
      )
      Nothing

deployNightParamsTxBalance
  :: forall era m
   . ( C.IsConwayBasedEra era
     , Convex.Class.MonadBlockchain era m
     , Convex.Class.MonadUtxoQuery m
     , MonadError (CoinSelection.BalanceTxError era) m
     , C.HasScriptLanguageInEra C.PlutusScriptV3 era
     )
  => C.Hash C.PaymentKey
  -> C.TxIn
  -> NightMintingParams
  -> m (C.BalancedTxBody era)
deployNightParamsTxBalance paymentKey ppInitTxOutRef nightProtocolParams = do
  networkId <- Convex.Class.queryNetworkId
  balanceTx paymentKey $
    initProtocolParams networkId ppInitTxOutRef nightProtocolParams

balanceTx
  :: forall era m a
   . ( C.IsConwayBasedEra era
     , Convex.Class.MonadBlockchain era m
     , Convex.Class.MonadUtxoQuery m
     , MonadError (CoinSelection.BalanceTxError era) m
     )
  => C.Hash C.PaymentKey
  -> BuildTxT era m a
  -> m (C.BalancedTxBody era)
balanceTx paymentKey btx = do
  params <- Convex.Class.queryProtocolParameters
  txBuilder <- execBuildTxT $ btx >> setMinAdaDepositAll params
  bteOperatorUtxos <-
    Utxos.toApiUtxo @era
      <$> Convex.Class.utxosByPaymentCredential (C.PaymentCredentialByKey paymentKey)
  output <- returnOutputFor (C.PaymentCredentialByKey paymentKey)
  (balancedTx, _) <-
    CoinSelection.balanceTx
      mempty
      output
      (Utxos.fromApiUtxo @era bteOperatorUtxos)
      txBuilder
      CoinSelection.TrailingChange
  pure balancedTx

deployNightParamsTxFull
  :: forall era m
   . ( C.IsConwayBasedEra era
     , Convex.Class.MonadBlockchain era m
     , Convex.Class.MonadUtxoQuery m
     , MonadError (CoinSelection.BalanceTxError era) m
     , C.HasScriptLanguageInEra C.PlutusScriptV3 era
     )
  => C.Hash C.PaymentKey
  -> C.TxIn
  -> NightMintingParams
  -> m (C.Tx era)
deployNightParamsTxFull paymentKey ppInitTxOutRef nightProtocolParams = do
  networkId <- Convex.Class.queryNetworkId
  balancedTx <-
    balanceTx paymentKey $
      initProtocolParams networkId ppInitTxOutRef nightProtocolParams
  pure $ Convex.CoinSelection.signBalancedTxBody [] balancedTx

updateProtocolParamsTxFull
  :: forall era m
   . ( C.IsConwayBasedEra era
     , C.HasScriptLanguageInEra C.PlutusScriptV3 era
     , Convex.Class.MonadBlockchain era m
     , Convex.Class.MonadUtxoQuery m
     , MonadError (CoinSelection.BalanceTxError era) m
     )
  => C.Hash C.PaymentKey
  -> C.TxIn
  -> C.PlutusScript C.PlutusScriptV3
  -> C.TxOut C.CtxUTxO era
  -> NightMintingParams
  -> [C.Hash C.PaymentKey]
  -> m (C.Tx era)
updateProtocolParamsTxFull paymentKey ppTxIn ppScript protocolParamsInput newNightProtocolParams signers = do
  networkId <- Convex.Class.queryNetworkId
  balancedTx <-
    balanceTx paymentKey $
      updateProtocolParams
        networkId
        ppTxIn
        ppScript
        protocolParamsInput
        newNightProtocolParams
        signers
  pure $ Convex.CoinSelection.signBalancedTxBody [] balancedTx

registerConwayStakeCredentials
  :: forall era m
   . (MonadBuildTx era m, C.IsConwayBasedEra era)
  => C.StakeCredential
  -> m ()
registerConwayStakeCredentials stakeCred = do
  let cert :: C.Certificate era =
        C.conwayEraOnwardsConstraints @era C.conwayBasedEra $
          C.ConwayCertificate C.conwayBasedEra $
            TxCert.RegTxCert $
              C.toShelleyStakeCredential stakeCred
  addCertificate cert

registerDynamicLogicStakeCredential
  :: (MonadBuildTx era m, C.IsConwayBasedEra era) => C.StakeCredential -> m ()
registerDynamicLogicStakeCredential = registerConwayStakeCredentials

registerDynamicLogicScriptTxFull
  :: forall era m
   . ( C.IsConwayBasedEra era
     , Convex.Class.MonadBlockchain era m
     , Convex.Class.MonadUtxoQuery m
     , MonadError (CoinSelection.BalanceTxError era) m
     )
  => C.Hash C.PaymentKey
  -> C.StakeCredential
  -> m (C.Tx era)
registerDynamicLogicScriptTxFull paymentKey dynamicMintingLogicStakeCredential = do
  balancedTx <-
    balanceTx paymentKey $
      registerDynamicLogicStakeCredential dynamicMintingLogicStakeCredential
  pure $ Convex.CoinSelection.signBalancedTxBody [] balancedTx