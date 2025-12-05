module Midnight.GlacierDrop.Contrib.MpfCliSpec where

import Codec.Serialise (deserialise)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as LBS (fromStrict)
import Data.Either (isRight)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, isJust)
import Data.Set as Set
import Data.Traversable (for)
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import GHC.IORef (newIORef, readIORef, writeIORef)
import Midnight.GlacierDrop.Contrib.MpfCli
import Midnight.GlacierDrop.Scripts.Debug.MerkleTrieFFICompile (
  compiledInsertCheckFunction,
 )
import Midnight.GlacierDrop.Scripts.MerkleTrieFFI.Types (
  TrieInsertRedeemer (TrieInsertRedeemer),
 )
import Midnight.GlacierDrop.Scripts.MerkleTrieFFI.Types qualified as M
import Midnight.GlacierDrop.Scripts.MerkleTrieFFI.Types qualified as MTFFI
import Midnight.GlacierDrop.Scripts.TestCommon.Eval
import PlutusLedgerApi.Common (
  BuiltinData,
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
  builtinDataToData,
 )
import PlutusLedgerApi.V2 (dataToBuiltinData)
import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Builtins.Internal qualified as P
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import System.Environment (lookupEnv)
import System.IO (hFlush, stdout)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString ()
import Text.Read (readMaybe)

readTestDataFromFile :: Bool
readTestDataFromFile = False

writeTestDataToFile :: Bool
writeTestDataToFile = False

makeBuiltinData :: BS.ByteString -> BuiltinData
makeBuiltinData =
  dataToBuiltinData
    . deserialise
    . LBS.fromStrict

-- Helper to create a temporary MPF database
withTempMpfDb :: (MpfCliDbPath -> IO a) -> IO a
withTempMpfDb action = do
  systemTempDir <- getTemporaryDirectory
  uuid <- nextRandom
  let testDirName = "mpf-test-" ++ UUID.toString uuid
      testDir = systemTempDir ++ "/" ++ testDirName
  result <- try $ action (MpfCliDbPath testDir)
  case result of
    Left (e :: SomeException) -> do
      putStrLn $ "Test failed. Keeping directory: " <> testDir
      putStrLn $ "Exception was: " <> show e
      throwIO e
    Right x -> do
      removeDirectoryRecursive testDir
      pure x

newtype Sha256Hash = Sha256Hash BS.ByteString
  deriving (Show, Eq)

instance Arbitrary Sha256Hash where
  arbitrary = Sha256Hash . BS.pack <$> vectorOf 32 arbitrary

-- Helper to read key-value pairs from file
readKeyValuePairsFromFile
  :: FilePath -> Int -> Int -> IO [(MpfCliKey, MpfCliValue)]
readKeyValuePairsFromFile filePath startIndex count = do
  content <- BS.readFile filePath
  let allLines = BS.split 10 content -- 10 is newline character
      skippedLines = Prelude.drop (startIndex * 2) allLines -- Skip to start index
      pairLines = Prelude.take (count * 2) skippedLines -- Each pair takes 2 lines
      pairs = toPairs pairLines
  pure pairs
  where
    toPairs [] = []
    toPairs [_] = [] -- Odd number of lines, skip the last one
    toPairs (k : v : rest) =
      case (B16.decode k, B16.decode v) of
        (Right kDecoded, Right vDecoded) -> (MpfCliKey kDecoded, MpfCliValue vDecoded) : toPairs rest
        _ -> error $ "Failed to decode hex strings: " <> show (k, v)

-- Helper to generate unique key-value pairs
generateUniqueKeyValuePairs
  :: Int -> Set BS.ByteString -> IO [(MpfCliKey, MpfCliValue)]
generateUniqueKeyValuePairs count existingKeys = do
  let go acc usedKeys
        | length acc == count = pure acc
        | otherwise = do
            key' <- generate arbitrary
            valueLength <- generate $ choose (0, 32)
            value' <- BS.pack <$> generate (vectorOf valueLength arbitrary)
            let Sha256Hash key = key'
            if Set.member key usedKeys
              then go acc usedKeys -- Try again with same usedKeys
              else go ((MpfCliKey key, MpfCliValue value') : acc) (Set.insert key usedKeys)
  go [] existingKeys

-- Test configuration
defaultMpfPath :: MpfCliPath
defaultMpfPath = MpfCliPath "mpf-cli-ts"

-- Use the below if the js offchain becomes a throughput bottleneck:
-- MpfCliPath
--   "/home/gumbo/iohk/mgdoc-claim-enablement/scripts/merkle-patricia-forestry/mpf-cli1" -- Assuming mpf-cli is in PATH

mkSpec :: IO Spec
mkSpec = do
  keySetRef <- newIORef mempty
  batchKeySetRef <- newIORef mempty
  pure $ do
    describe "MpfCli operations" $ do
      around withTempMpfDb $ it "should successfully insert and verify existence of a node" $ \dbPath -> do
        let client = mkMpfClient defaultMpfPath dbPath
            key = MpfCliKey "0000"
            value = MpfCliValue "0000"

        -- Insert a node
        insertResult <- client.insert key value
        case insertResult of
          Left err ->
            expectationFailure $ "Insert failed with error: " <> err
          Right (MpfCliResponseError err) ->
            expectationFailure $ "Insert returned error response: " <> err
          Right (MpfCliResponseSuccess resp) -> do
            -- Verify the response structure
            resp.newHash `shouldSatisfy` (not . BS.null . unMpfRootHash)
            resp.proofResp.verified `shouldBe` True

            -- Check existence
            existsResult <- client.exists key
            case existsResult of
              Left err ->
                expectationFailure $ "Exists check failed with error: " <> err
              Right (MpfCliResponseError err) ->
                expectationFailure $ "Exists check returned error response: " <> err
              Right (MpfCliResponseSuccess _) -> pure ()

            -- Verify proof
            proofResult <- client.mkMembershipProof key
            case proofResult of
              Left err ->
                expectationFailure $ "Proof verification failed with error: " <> err
              Right (MpfCliResponseError err) ->
                expectationFailure $ "Proof verification returned error response: " <> err
              Right (MpfCliResponseSuccess proof) -> do
                proof.verified `shouldBe` True
                proof.proofCBOR `shouldSatisfy` (not . BS.null)

      around withTempMpfDb $ it "should handle non-existent nodes correctly" $ \dbPath -> do
        let key = MpfCliKey "non-existent-key"

        let client = mkMpfClient defaultMpfPath dbPath
        existsResult <- client.exists key
        existsResult `shouldSatisfy` isRight
        case existsResult of
          Right (MpfCliResponseError _) -> pure ()
          _ -> expectationFailure "Should return error for non-existent node"

      around withTempMpfDb $ it "should handle multiple insertions" $ \dbPath -> do
        let pairs =
              [ (MpfCliKey "key1", MpfCliValue "value1")
              , (MpfCliKey "key2", MpfCliValue "value2")
              , (MpfCliKey "key3", MpfCliValue "value3")
              ]

        let client = mkMpfClient defaultMpfPath dbPath
        results <- for pairs (uncurry client.insert)

        all isRight results `shouldBe` True

        existsResults <-
          traverse
            (\(k, _) -> client.exists k)
            pairs

        all isRight existsResults `shouldBe` True
        all (\case Right (MpfCliResponseSuccess _) -> True; _ -> False) existsResults
          `shouldBe` True

      -- If we wrap below test in `around withTempMpfDb`, it will create a new temporary database for each test run!
      it "should handle arbitrary bytestring keys and values" do
        let dbPath = MpfCliDbPath "property-test-mpf-db"
        withMaxSuccess 10 $ property $ \(Sha256Hash key', value') -> do
          let key = MpfCliKey key'
              value = MpfCliValue value'

          let client = mkMpfClient defaultMpfPath dbPath
          insertResult <- client.insert key value
          insertResult `shouldSatisfy` isRight

          case insertResult of
            Left err -> expectationFailure $ "Insert failed with error: " <> err
            Right (MpfCliResponseError err) -> expectationFailure $ "Insert returned error response: " <> err
            Right (MpfCliResponseSuccess (resp :: InsertNodeResponse)) -> do
              let builtinData = makeBuiltinData resp.proofResp.proofCBOR
              let proofSteps = PlutusTx.fromBuiltinData builtinData :: Maybe [MTFFI.ProofStep]
              proofSteps `shouldSatisfy` isJust

          existsResult <- client.exists key
          existsResult `shouldSatisfy` isRight
          case existsResult of
            Right (MpfCliResponseSuccess _) -> pure ()
            _ -> expectationFailure "Node should exist"

      let plutusInsertCheckProgram rdmr =
            applyArguments
              (fromCardanoApiScriptToProgram compiledInsertCheckFunction)
              rdmr
      it "should evaluate compiled insert function" do
        let dbPath = MpfCliDbPath "pinsert-test"
        for_ [1 .. 10] \(_ :: Int) -> do
          set <- readIORef keySetRef
          [(key@(MpfCliKey key'), value@(MpfCliValue value'))] <-
            generateUniqueKeyValuePairs 1 set
          writeIORef keySetRef (Set.insert key' set)
          putStrLn "insert key val pair to insert:"
          print (key', value')
          hFlush stdout
          let client = mkMpfClient defaultMpfPath dbPath
          insertResult <- client.insert key value
          insertResult `shouldSatisfy` isRight
          case insertResult of
            Left err -> expectationFailure $ "Insert failed with error: " <> err
            Right (MpfCliResponseError err) -> expectationFailure $ "Insert returned error response: " <> err
            Right (MpfCliResponseSuccess (resp :: InsertNodeResponse)) -> do
              putStrLn "insert node response:"
              print resp
              let possibleProof = fromBuiltinData $ makeBuiltinData resp.proofResp.proofCBOR
              case possibleProof of
                Nothing -> expectationFailure "Failed to decode proof steps from BuiltinData"
                Just proof -> do
                  let redeemer = do
                        let outputRoot = M.MerklePatriciaForestry $ P.BuiltinByteString (unMpfRootHash resp.newHash)
                            inputRoot = M.MerklePatriciaForestry $ P.BuiltinByteString $ do
                              unMpfRootHash (fromMaybe emptyRootHash resp.prevHash)
                            inputKey = P.BuiltinByteString key'
                            inputValue = P.BuiltinByteString value'
                        TrieInsertRedeemer outputRoot inputRoot inputKey inputValue proof
                      args = [builtinDataToData . toBuiltinData $ redeemer]
                  -- Error out to ensure the Mpf DB is in the correct state to debug the failure.
                  expectScriptSucceedAndTraceWithFailureAndStop
                    (plutusInsertCheckProgram args)
                    ("redeemer: " <> show redeemer)

          existsResult <- client.exists key
          existsResult `shouldSatisfy` isRight
          case existsResult of
            Right (MpfCliResponseSuccess _) -> pure ()
            Right (MpfCliResponseError err) ->
              expectationFailure $ "Exists check returned error response: " <> err
            _ -> expectationFailure "Node should exist"

      it "should evaluate compiled insert function for batches" do
        let dbPath = MpfCliDbPath "batch-insert-test"
            batchSize = 1_000
            defaultMaxBatches = 5
        envVal <- lookupEnv "MAX_MPF_CLI_BATCH_SIZE"
        let maxBatches = fromMaybe defaultMaxBatches (envVal >>= readMaybe)

        for_ [1 .. maxBatches] $ \(idx :: Int) -> do
          putStrLn $ "Inserting batch nr " <> show idx <> " / " <> show maxBatches
          set <- readIORef batchKeySetRef

          -- Generate batch of unique keys and values
          keyValuePairs <-
            if readTestDataFromFile
              then
                readKeyValuePairsFromFile
                  "key_value_pairs.txt"
                  ((idx - 1) * batchSize)
                  batchSize
              else generateUniqueKeyValuePairs batchSize set
          let batch = MpfCliInsertBatch keyValuePairs

          -- Write key-value pairs to file
          when writeTestDataToFile $ do
            let kvBytes =
                  BS.concat
                    [ B16.encode k <> "\n" <> B16.encode v <> "\n"
                    | (MpfCliKey k, MpfCliValue v) <- keyValuePairs
                    ]
            if idx == 1
              then BS.writeFile "key_value_pairs.txt" kvBytes
              else BS.appendFile "key_value_pairs.txt" kvBytes

          -- Update the key set with new keys
          let newKeys = Set.fromList $ fmap (unMpfCliKey . fst) keyValuePairs
          writeIORef batchKeySetRef (Set.union set newKeys)

          let client = mkMpfClient defaultMpfPath dbPath
          -- get now time
          (batchResult, elapsed) <- client.insertBatchWithElapsed batch
          putStrLn $ "Batch insert took: " <> show elapsed <> " seconds"
          batchResult `shouldSatisfy` isRight

          case batchResult of
            Left err -> expectationFailure $ "Batch insert failed with error: " <> err
            Right (MpfCliResponseError err) -> expectationFailure $ "Batch insert returned error response: " <> err
            Right (MpfCliResponseSuccess responses) -> do
              -- Verify each response with the compiled check function
              for_ (zip keyValuePairs responses) $ \((key, value), resp) -> do
                let possibleProof = fromBuiltinData $ makeBuiltinData resp.proofResp.proofCBOR
                -- let keyHex = B16.encode $ unMpfCliKey key
                --     valueHex = B16.encode $ unMpfCliValue value
                --     proofHex = B16.encode resp.proofResp.proofCBOR
                -- putStrLn $ "Processing key-value pair: " <> show (keyHex, valueHex, proofHex)

                case possibleProof of
                  Nothing -> expectationFailure "Failed to decode proof steps from BuiltinData"
                  Just proof -> do
                    let redeemer = do
                          let outputRoot = M.MerklePatriciaForestry $ P.BuiltinByteString (unMpfRootHash resp.newHash)
                              inputRoot =
                                M.MerklePatriciaForestry $
                                  P.BuiltinByteString $
                                    unMpfRootHash (fromMaybe emptyRootHash resp.prevHash)
                              inputKey = P.BuiltinByteString (unMpfCliKey key)
                              inputValue = P.BuiltinByteString (unMpfCliValue value)
                          TrieInsertRedeemer outputRoot inputRoot inputKey inputValue proof
                        args = [builtinDataToData . toBuiltinData $ redeemer]
                    expectScriptSucceedAndTraceWithFailure
                      (plutusInsertCheckProgram args)
                      ( "redeemer: "
                          <> show redeemer
                          <> "\nSerialized redeemer: "
                          <> show (BI.serialiseData $ PlutusTx.toBuiltinData redeemer)
                          <> "\noutputRoot: "
                          <> show (B16.encode (getTreeRoot $ MTFFI.outputRoot redeemer))
                          <> "\ninputRoot: "
                          <> show (B16.encode (getTreeRoot $ MTFFI.inputRoot redeemer))
                          <> "\ninputKey: "
                          <> show (B16.encode (BI.fromBuiltin $ MTFFI.inputKey redeemer))
                          <> "\ninputValue: "
                          <> show (B16.encode (BI.fromBuiltin $ MTFFI.inputValue redeemer))
                      )

              let keys = fmap fst keyValuePairs
              existsResults <- client.batchExists keys
              existsResults `shouldSatisfy` isRight
              case existsResults of
                Right (MpfCliResponseSuccess _) ->
                  putStrLn $
                    "Batch existence check successful for " <> show (length keys) <> " keys"
                Right (MpfCliResponseError err) ->
                  expectationFailure $ "Batch exists check returned error response: " <> err
                Left err -> expectationFailure $ "Batch exists check failed with error: " <> err
  where
    getTreeRoot :: MTFFI.MerklePatriciaForestry -> BS.ByteString
    getTreeRoot (M.MerklePatriciaForestry bs) = BI.fromBuiltin bs
