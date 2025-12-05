{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Midnight.GlacierDrop.Transactions.NightMintFull where

import Cardano.Api qualified as C
import Control.Monad (forM_)
import Convex.BuildTx
import Convex.PlutusLedger.V1 qualified as PL
import Convex.PlutusLedger.V3 qualified as PL3
import Data.Aeson
import Data.Aeson.Types qualified as JSON
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text.Encoding qualified as TE
import GHC.Exts (fromList)
import GHC.Generics (Generic)
import Midnight.GlacierDrop.Api (MerkleHash (..), Sha512HalfScriptHash (..))
import Midnight.GlacierDrop.Scripts.Common (emptyTrieRoot, starToken, toBurnTokenName, newNightTokenName)
import Midnight.GlacierDrop.Scripts.HydraThread.Types
import Midnight.GlacierDrop.Scripts.HydraThreadCompile (
  sha512HalfProxyPlutusScriptHash,
  versionedHydraThreadScript,
 )
import Midnight.GlacierDrop.Scripts.MerkleTrieFFICompile (
  insertTrieRewardingPlutusCredential,
 )
import Midnight.GlacierDrop.Scripts.Night.Types (
  NightMintRedeemer (NightMintStar),
  NightParams (..),
 )
import Midnight.GlacierDrop.Scripts.NightCompile (versionedNightScript)
import Midnight.GlacierDrop.Scripts.ProtocolParams.Types qualified as NP
import Midnight.GlacierDrop.Scripts.ProtocolParamsCompile (
  protocolParamsCurrencySymbol,
  versionedProtocolParamsScript,
 )
import Midnight.GlacierDrop.Scripts.RedemptionCompile (
  redemptionPlutusScriptHash,
  versionedRedemptionScript,
 )
import Midnight.GlacierDrop.Scripts.RedemptionSupply.Types
import Midnight.GlacierDrop.Scripts.RedemptionSupplyCompile (
  versionedRedemptionSupplyScript,
 )
import Midnight.GlacierDrop.Transactions.Common (
  hydraThreadToken,
 )
import Midnight.GlacierDrop.Transactions.NightProtocolParams.Aeson ()
import Numeric.Natural (Natural)
import PlutusLedgerApi.V3 qualified as PV3
import Refined (makeAscending)
import Midnight.GlacierDrop.Scripts.NightCustom.Terms
import Midnight.GlacierDrop.Scripts.NightCustom.Types

data NightMintEnv = NightMintEnv
  { nmSeedTxIn :: C.TxIn
  , nmParamsInitTxIn :: C.TxIn
  , nmNightMintingScript :: C.PlutusScript C.PlutusScriptV3
  , nmRedemptionSupplyScript :: C.PlutusScript C.PlutusScriptV3
  , nmRedemptionScript :: C.PlutusScript C.PlutusScriptV3
  , nmHydraThreadScript :: C.PlutusScript C.PlutusScriptV3
  , nmNightParamsScript :: C.PlutusScript C.PlutusScriptV3
  , nmNightParams :: NightParams
  }
  deriving (Generic)

instance ToJSONKey (C.Address C.ShelleyAddr) where
  toJSONKey = JSON.toJSONKeyText C.serialiseAddress

instance FromJSONKey (C.Address C.ShelleyAddr) where
  fromJSONKey = JSON.FromJSONKeyTextParser $ \addrText ->
    case C.deserialiseAddress (C.AsAddress C.AsShelleyAddr) addrText of
      Just addr -> pure addr
      Nothing -> fail $ "Failed to deserialise address: " <> show addrText

newtype MerkleRootHash = MerkleRootHash {unMerkleRootHash :: ByteString}
  deriving (Generic, Show)

instance ToJSON MerkleRootHash where
  toJSON = String . TE.decodeUtf8 . Base16.encode . unMerkleRootHash

instance FromJSON MerkleRootHash where
  parseJSON = withText "MerkleRootHash" $ \txt ->
    case Base16.decode (TE.encodeUtf8 txt) of
      Left err -> fail $ "Invalid hex: " <> err
      Right bs -> pure (MerkleRootHash bs)

newtype MessageBytes = MessageBytes {unMessageBytes :: ByteString}
  deriving (Generic, Show)

instance ToJSON MessageBytes where
  toJSON = String . TE.decodeUtf8 . Base16.encode . unMessageBytes

instance FromJSON MessageBytes where
  parseJSON = withText "MessageBytes" $ \txt ->
    case Base16.decode (TE.encodeUtf8 txt) of
      Left err -> fail $ "Invalid hex: " <> err
      Right bs -> pure (MessageBytes bs)

data NightMintArgs = NightMintArgs
  { nmaRedemptionShardDepth :: Natural
  , nmaRedemptionIncrements :: Natural
  , nmaMessagePrefix :: MessageBytes
  , nmaMessageSuffix :: MessageBytes
  , nmaHydraCommitScriptHash :: C.ScriptHash
  , nmaMerkleRoot :: MerkleRootHash
  , nmaTgeAgentAuthKeys :: Set.Set (C.Hash C.PaymentKey)
  , nmaFoundationWallets :: Map.Map (C.Address C.ShelleyAddr) Natural
  , nmaTgeWallet :: C.Address C.ShelleyAddr
  , nmaMinAuthSignatures :: Natural
  , nmaMinTreasuryTokens :: Natural
  , nmaSupply :: Natural
  , nmaAllocationCount :: Natural
  , nmaJitterStrataCount :: Natural
  , nmaNightParamsInitTxIn :: C.TxIn
  , nmaSeedTxIn :: C.TxIn
  }
  deriving (Generic, ToJSON, FromJSON, Show)

mkNightMintEnv :: NightMintArgs -> NightMintEnv
mkNightMintEnv
  NightMintArgs
    { nmaRedemptionIncrements
    , nmaRedemptionShardDepth
    , nmaMessagePrefix
    , nmaMessageSuffix
    , nmaHydraCommitScriptHash
    , nmaMerkleRoot
    , nmaTgeAgentAuthKeys
    , nmaTgeWallet
    , nmaMinAuthSignatures
    , nmaMinTreasuryTokens
    , nmaSupply
    , nmaAllocationCount
    , nmaJitterStrataCount
    , nmaNightParamsInitTxIn
    , nmaSeedTxIn
    } =
    let agentAuthKeys = makeAscending $ PL.transPubKeyHash <$> Set.toList nmaTgeAgentAuthKeys
        hydraThreadParams =
          HydraThreadParams
            { hydraCommitScriptHash = PL.transScriptHash nmaHydraCommitScriptHash
            , messagePrefix = PV3.toBuiltin (unMessageBytes nmaMessagePrefix)
            , messageSuffix = PV3.toBuiltin (unMessageBytes nmaMessageSuffix)
            , tgeAgentAuthKeys = agentAuthKeys
            , minAuthSignatures = fromIntegral nmaMinAuthSignatures
            , sha512HalfScriptHash = Sha512HalfScriptHash sha512HalfProxyPlutusScriptHash
            , trieInsertCred = insertTrieRewardingPlutusCredential
            }
        hydraThreadScript = versionedHydraThreadScript hydraThreadParams
        redemptionSupplyParams =
          RedemptionSupplyParams
            { redemptionScriptHash = redemptionPlutusScriptHash
            , jitterStrataCount = fromIntegral nmaJitterStrataCount
            , redemptionShardDepth =
                RedemptionShardDepth $ MerkleTreeDepth $ fromIntegral nmaRedemptionShardDepth
            , redemptionIncrements = TotalIncrements (fromIntegral nmaRedemptionIncrements)
            , tgeWalletAddress = PL.transAddressShelley nmaTgeWallet
            }
        redemptionSupplyScript = versionedRedemptionSupplyScript redemptionSupplyParams

        nightParamsScript = versionedProtocolParamsScript $ PL3.transTxOutRef nmaNightParamsInitTxIn
        nightParamsCurrencySymbol = protocolParamsCurrencySymbol $ PL3.transTxOutRef nmaNightParamsInitTxIn
        nightParams =
          NightParams
            { merkleRoot = MerkleHash (PV3.toBuiltin (unMerkleRootHash nmaMerkleRoot))
            , treeDepth = ceiling $ logBase @Double 2 $ fromIntegral nmaAllocationCount
            , seedInput = PL3.transTxOutRef nmaSeedTxIn
            , minTreasuryTokens = fromIntegral nmaMinTreasuryTokens
            , supply = fromIntegral nmaSupply
            , allocationCount = fromIntegral nmaAllocationCount
            , jitterStrataCount = fromIntegral nmaJitterStrataCount
            , protocolParamsCS = nightParamsCurrencySymbol
            }
     in NightMintEnv
          { nmSeedTxIn = nmaSeedTxIn
          , nmParamsInitTxIn = nmaNightParamsInitTxIn
          , nmNightMintingScript = versionedNightScript nightParams
          , nmRedemptionSupplyScript = redemptionSupplyScript
          , nmRedemptionScript = versionedRedemptionScript
          , nmHydraThreadScript = hydraThreadScript
          , nmNightParamsScript = nightParamsScript
          , nmNightParams = nightParams
          }

nightMintTx
  :: ( MonadBuildTx era m
     , C.IsBabbageBasedEra era
     , C.HasScriptLanguageInEra C.PlutusScriptV3 era
     )
  => C.NetworkId
  -> C.TxIn
  -> NightMintArgs
  -> NP.NightMintingParams
  -> [PV3.PubKeyHash]
  -> m ()
nightMintTx networkId nightParamsTxIn nightMintArgs nightMintingParams requiredSigners = do
  let ( either (error . show) id . PL.unTransScriptHash . NP.hydraThreadScriptHash ->
          hydraThreadScriptHash'
        ) = nightMintingParams
  let NightMintEnv
        { nmSeedTxIn
        , nmNightMintingScript
        } = mkNightMintEnv nightMintArgs
  let NightMintArgs
        { nmaMerkleRoot
        , nmaAllocationCount
        , nmaSupply
        } = nightMintArgs

  spendPublicKeyOutput nmSeedTxIn
  addReference nightParamsTxIn

  -- Mint the night protocol params NFT
  mintPlutus
    nmNightMintingScript
    NightMintStar
    (PL.unTransAssetName starToken)
    (fromIntegral nmaSupply)
  mintPlutus
    nmNightMintingScript
    NightMintStar
    (PL.unTransAssetName hydraThreadToken)
    1

  -- Add the night protocol params output with the NFT
  addNightOutput networkId nmNightMintingScript (fromIntegral nmaSupply)

  forM_ requiredSigners $ \signer ->
    addRequiredSignature (either (error . show) id $ PL.unTransPubKeyHash signer)

  let hydraThreadClaiming =
        HydraThreadClaiming $
          MakeHydraThreadClaiming
            { allotmentsTreeRoot = MerkleHash (PV3.toBuiltin (unMerkleRootHash nmaMerkleRoot))
            , allotmentsTreeDepth =
                ceiling $ logBase @Double 2 $ fromIntegral nmaAllocationCount
            , alreadyClaimedTrieRoot = emptyTrieRoot
            , redemptionsSum = 0
            }
  addHydraThreadOutput
    networkId
    hydraThreadScriptHash'
    nmNightMintingScript
    hydraThreadClaiming
  pure ()

addHydraThreadOutput
  :: (MonadBuildTx era m, C.IsBabbageBasedEra era)
  => C.NetworkId
  -> C.ScriptHash
  -> C.PlutusScript C.PlutusScriptV3
  -> HydraThreadDatum
  -> m ()
addHydraThreadOutput nid hydraThreadScriptHash nmNightMintingScript hydraThreadClaiming =
  payToScriptInlineDatum
    nid
    hydraThreadScriptHash
    hydraThreadClaiming
    C.NoStakeAddress
    ( nightTokenValue
        nmNightMintingScript
        (PL.unTransAssetName hydraThreadToken)
        (C.Quantity 1)
    )

addNightOutput
  :: (MonadBuildTx era m, C.IsBabbageBasedEra era)
  => C.NetworkId
  -> C.PlutusScript C.PlutusScriptV3
  -> Integer
  -> m ()
addNightOutput nid nmNightMintingScript supply =
  payToScriptInlineDatum
    nid
    (C.hashScript $ C.PlutusScript C.plutusScriptVersion nmNightMintingScript)
    ()
    C.NoStakeAddress
    ( nightTokenValue
        nmNightMintingScript
        (PL.unTransAssetName starToken)
        (C.Quantity supply)
    )

nightTokenValue
  :: C.PlutusScript C.PlutusScriptV3 -> C.AssetName -> C.Quantity -> C.Value
nightTokenValue mintScript assetName n =
  let policyId = C.scriptPolicyId $ C.PlutusScript C.plutusScriptVersion mintScript
   in fromList [(C.AssetId policyId assetName, n)]

dynamicNightBurnGenesisTx
  :: forall era m
   . ( MonadBuildTx era m
     , C.IsBabbageBasedEra era
     , C.HasScriptLanguageInEra C.PlutusScriptV3 era
     )
  => C.NetworkId
  -> C.TxIn
  -> C.TxOut C.CtxUTxO era
  -> C.PlutusScript C.PlutusScriptV3
  -> Maybe C.TxIn
  -> C.TxIn
  -> C.TxOut C.CtxUTxO era
  -> C.PlutusScript C.PlutusScriptV3
  -> Maybe C.TxIn
  -> C.StakeAddressReference
  -> [C.Hash C.PaymentKey]
  -> m ()
dynamicNightBurnGenesisTx
  dnbNetworkId
  dnbNightTxIn
  dnbNightInput
  dnbNightMintingScript
  dnbNightScriptReference
  dnbNightParamsTxIn
  dnbNightParamsUTxO
  dnbDynamicMintingLogicScript
  dnbDynamicMintingLogicRef
  dnbStakeAddress
  dnbRequiredSigners =
    do
      addReference dnbNightParamsTxIn
      maybe (pure ()) addReference dnbNightScriptReference
      maybe (pure ()) addReference dnbDynamicMintingLogicRef
      forM_ dnbRequiredSigners addRequiredSignature

      spendNightInput
      addDynamicLogicWithdrawal

      when (toBurnQuantity <= 0) $
        error "dynamicNightBurnGenesisTx: missing ToBurn marker"
      when (totalNightTokens <= 0) $
        error "dynamicNightBurnGenesisTx: no NIGHT tokens found at night input"
      when
        ( length (filter (`elem` tgeAgentAuthKeys) dnbRequiredSigners)
            < fromIntegral minAuthSignatures
        )
        $ error "dynamicNightBurnGenesisTx: insufficient required signatures"

      case dnbNightScriptReference of
        Just ref ->
          mintPlutusRef
            ref
            C.PlutusScriptV3
            nightScriptHash
            DynamicMintingLogic
            toBurnAssetName
            (-1)
        Nothing ->
          mintPlutus
            dnbNightMintingScript
            DynamicMintingLogic
            toBurnAssetName
            (-1)

      addRedemptionSupplyOutput
      when (treasuryTokens > 0) addTreasuryOutput
      mapM_ addFoundationOutput foundationWalletsAdjusted
    where
      nightInputValue = Lens.view (L._TxOut . Lens._2 . L._TxOutValue) dnbNightInput
      nightScriptHash = C.hashScript $ C.PlutusScript C.PlutusScriptV3 dnbNightMintingScript
      nightPolicyId = C.PolicyId nightScriptHash
      nightAssetId = C.AssetId nightPolicyId (PL1.unTransAssetName newNightTokenName)
      toBurnAssetId = C.AssetId nightPolicyId toBurnAssetName

      C.Quantity totalNightTokens = C.selectAsset nightInputValue nightAssetId
      C.Quantity toBurnQuantity = C.selectAsset nightInputValue toBurnAssetId

      spendNightInput =
        case dnbNightScriptReference of
          Just ref ->
            spendPlutusRefWithInlineDatum
              dnbNightTxIn
              ref
              C.PlutusScriptV3
              ()
          Nothing ->
            spendPlutusInlineDatum
              dnbNightTxIn
              dnbNightMintingScript
              ()

      addDynamicLogicWithdrawal = do
        let scriptHash =
              C.hashScript $
                C.PlutusScript C.PlutusScriptV3 dnbDynamicMintingLogicScript
            dynamicStakeCredential = C.StakeCredentialByScript scriptHash
            witness =
              maybe
                (buildScriptWitness dnbDynamicMintingLogicScript C.NoScriptDatumForStake ())
                (\ref -> buildRefScriptWitness ref C.PlutusScriptV3 C.NoScriptDatumForStake ())
                dnbDynamicMintingLogicRef
        addWithdrawal
          (C.makeStakeAddress dnbNetworkId dynamicStakeCredential)
          (C.Quantity 0)
          (C.ScriptWitness C.ScriptWitnessForStakeAddr witness)

      hydraFinal =
        case hardcodedHydraThreadDatum of
          HT.HydraThreadFinal finalDatum -> finalDatum
          _ -> error "dynamicNightBurnGenesisTx: expected hydra final datum"
      HT.MakeHydraThreadFinal
        { HT.redemptionsSum = hydraSum
        , HT.redemptionsTreeRoot = hydraTreeRoot
        , HT.redemptionsTreeDepth = hydraTreeDepth
        } = hydraFinal
      effectiveRedemptionSum = getStar hydraSum

      foundationWalletsConverted :: [(C.Address C.ShelleyAddr, Integer)]
      foundationWalletsConverted = fmap (Data.Bifunctor.first convertShelleyAddress) foundationWalletsPlutus

      convertShelleyAddress addr =
        either
          (error . (<> "dynamicNightBurnGenesisTx: invalid address") . show)
          id
          (PL1.unTransAddressShelley dnbNetworkId addr)

      convertSupplyAddress address =
        either
          (error . (<> "dynamicNightBurnGenesisTx: invalid supply script address") . show)
          id
          (PL1.unTransAddressShelley dnbNetworkId address)

      nightMintingParams = extractNightMintingParams dnbNightParamsUTxO

      NightMintingParams
        { foundationWallets = foundationWalletsPlutus
        , supplyScriptAddress = supplyScriptAddressPlutus
        , tgeAgentAuthKeys = tgeAgentAuthKeysPlutus
        , minAuthSignatures
        } = nightMintingParams

      supplyScriptAddress =
        convertSupplyAddress supplyScriptAddressPlutus

      tgeAgentAuthKeys =
        fmap
          ( either
              (error . (<> "dynamicNightBurnGenesisTx: invalid TGE key") . show)
              id
          )
          (PL1.unTransPubKeyHash <$> tgeAgentAuthKeysPlutus)

      nightRemaining = totalNightTokens - effectiveRedemptionSum
      foundationTotalAdjusted = nightRemaining - hardcodedMinTreasuryTokens
      foundationTotalUnadjusted = foldl' (+) 0 (snd <$> foundationWalletsConverted)
      foundationWalletsAdjusted
        | foundationTotalAdjusted <= 0 = []
        | foundationTotalUnadjusted <= 0 = []
        | foundationTotalAdjusted < foundationTotalUnadjusted =
            [ (address, (share * foundationTotalAdjusted) `div` foundationTotalUnadjusted)
            | (address, share) <- foundationWalletsConverted
            ]
        | otherwise = foundationWalletsConverted
      treasuryTokens =
        nightRemaining - foldl' (+) 0 (snd <$> foundationWalletsAdjusted)

      addRedemptionSupplyOutput =
        addOutput $
          C.TxOut
            (C.AddressInEra (C.ShelleyAddressInEra C.shelleyBasedEra) supplyScriptAddress)
            (mkTxOutValue $ fromList [(nightAssetId, C.Quantity effectiveRedemptionSum)])
            ( C.TxOutDatumInline C.babbageBasedEra $
                toHashableScriptData $
                  RedemptionSupplyDatum
                    { rootHash = hydraTreeRoot
                    , genesisTimestamp =
                        GenesisTimestamp hardcodedGenesisTimestamp
                    , redemptionIncrementPeriod = hardcodedRedemptionIncrementPeriod
                    , state = Subdividing hydraTreeDepth
                    }
            )
            C.ReferenceScriptNone

      addTreasuryOutput =
        addOutput $
          C.TxOut
            ( C.AddressInEra
                (C.ShelleyAddressInEra C.shelleyBasedEra)
                ( C.makeShelleyAddress
                    dnbNetworkId
                    (C.PaymentCredentialByScript hardcodedTreasuryScriptHash)
                    dnbStakeAddress
                )
            )
            (mkTxOutValue $ fromList [(nightAssetId, C.Quantity treasuryTokens)])
            C.TxOutDatumNone
            C.ReferenceScriptNone

      addFoundationOutput (address, amount)
        | amount <= 0 = pure ()
        | otherwise =
            addOutput $
              C.TxOut
                (C.AddressInEra (C.ShelleyAddressInEra C.shelleyBasedEra) address)
                (mkTxOutValue $ fromList [(nightAssetId, C.Quantity amount)])
                C.TxOutDatumNone
                C.ReferenceScriptNone

hardcodedHydraThreadDatum :: HT.HydraThreadDatum
hardcodedHydraThreadDatum =
  HT.HydraThreadFinal
    HT.MakeHydraThreadFinal
      { HT.finalTransactionId =
          PV3.TxId $
            BI.stringToBuiltinByteStringHex
              "559dc63cb1af905192154e1e5e5cec67537f4c3d1c8f9f43827b7bdd8777692e"
      , HT.redemptionsTreeRoot = hardcodedRedemptionsTreeRoot
      , HT.redemptionsTreeDepth = MerkleTreeDepth 20
      , HT.redemptionsSum = Star hardcodedEffectiveRedemptionSum
      }

posixTimeToPlutus :: POSIXTime -> PV3.POSIXTime
posixTimeToPlutus t =
  let millis = floor ((realToFrac t :: Rational) * 1000)
   in PV3.POSIXTime millis

hardcodedGenesisTimestamp :: PV3.POSIXTime
hardcodedGenesisTimestamp = _hardcodedGenesisTimestamp

hardcodedTreasuryScriptHash :: C.ScriptHash
hardcodedTreasuryScriptHash =
  case C.deserialiseFromRawBytesHex @C.ScriptHash
    (BSC.pack "77ef73922374fd16696fc8a5664b87f0db24eed0a29ae9f10893552c") of
    Right h -> h
    Left err ->
      error $
        "dynamicNightBurnGenesisTx: invalid hardcoded treasury script hash: "
          <> show err

toBurnAssetName :: C.AssetName
toBurnAssetName = PL.unTransAssetName toBurnTokenName
