{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (void)
import Data.Int (Int32)
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Effectful.HPQTypes
import System.Environment (lookupEnv)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "tests"
    [ testCase "test getLastQuery" testGetLastQuery
    , testCase "test withFrozenLastQuery" testWithFrozenLastQuery
    --    , testCase "test connection stats retrieval with new connection" testConnectionStatsWithNewConnection
    ]

testGetLastQuery :: Assertion
testGetLastQuery = do
  dbUrl <- getConnString
  let connectionSource = simpleSource $ defaultConnectionSettings {csConnInfo = dbUrl}
  void . runEff . runErrorNoCallStack @HPQTypesError . runDB (unConnectionSource connectionSource) defaultTransactionSettings $ do
    do
      -- Run the first query and perform some basic sanity checks
      let sql = mkSQL "SELECT 1"
      rowNo <- runQuery sql
      liftIO $ assertEqual "One row should be retrieved" 1 rowNo
      result <- fetchMany (runIdentity @Int32)
      liftIO $ assertEqual "Result should be [1]" [1] result
      (_, SomeSQL lastQuery) <- getLastQuery
      liftIO $ assertEqual "SQL don't match" (show sql) (show lastQuery)
    do
      -- Run the second query and check that `getLastQuery` gives updated result
      let newSQL = mkSQL "SELECT 2"
      runQuery_ newSQL
      (_, SomeSQL newLastQuery) <- getLastQuery
      liftIO $ assertEqual "SQL don't match" (show newSQL) (show newLastQuery)

testWithFrozenLastQuery :: Assertion
testWithFrozenLastQuery = do
  dbUrl <- getConnString
  let connectionSource = simpleSource $ defaultConnectionSettings {csConnInfo = dbUrl}
  void . runEff . runErrorNoCallStack @HPQTypesError . runDB (unConnectionSource connectionSource) defaultTransactionSettings $ do
    let sql = mkSQL "SELECT 1"
    runQuery_ sql
    withFrozenLastQuery $ do
      runQuery_ $ mkSQL "SELECT 2"
      getLastQuery >>= \(_, SomeSQL lastQuery) ->
        liftIO $ assertEqual "The last query before freeze should be reported" (show sql) (show lastQuery)
    getLastQuery >>= \(_, SomeSQL lastQuery) ->
      liftIO $ assertEqual "The last query before freeze should be reported" (show sql) (show lastQuery)

{-
testConnectionStatsWithNewConnection :: Assertion
testConnectionStatsWithNewConnection = do
  dbUrl <- getConnString
  let connectionSource = simpleSource $ defaultConnectionSettings {csConnInfo = dbUrl}
  void . runEff . runErrorNoCallStack @HPQTypesError . runDB (unConnectionSource connectionSource) defaultTransactionSettings . unsafeWithoutTransaction $ do
    do
      runQuery_ $ mkSQL "SELECT 1"
      runQuery_ $ mkSQL "SELECT 2"
      connectionStats <- getConnectionStats
      liftIO $ assertEqual "Incorrect connection stats" (ConnectionStats 3 3 3 0) connectionStats
    do
      runQuery_ $ mkSQL "CREATE TABLE some_table (field INT)"
      runQuery_ $ mkSQL "BEGIN"
      runQuery_ $ mkSQL "INSERT INTO some_table VALUES (1)"
      withNewConnection $ do
        connectionStats <- getConnectionStats
        liftIO $ assertEqual "Connection stats should be reset" (ConnectionStats 1 1 1 0) connectionStats
        noOfResults <- runQuery $ mkSQL "SELECT * FROM some_table"
        liftIO $ assertEqual "Results should not be visible yet" 0 noOfResults
      runQuery_ $ mkSQL "COMMIT"
      noOfResults <- runQuery $ mkSQL "SELECT * FROM some_table"
      liftIO $ assertEqual "Results should be visible" 1 noOfResults
      runQuery_ $ mkSQL "DROP TABLE some_table"
-}

----------------------------------------
-- Helpers

getConnString :: IO T.Text
getConnString =
  lookupEnv "GITHUB_ACTIONS" >>= \case
    Just "true" -> pure . T.pack $ "host=postgres user=postgres password=postgres"
    _ -> do
      lookupEnv "DATABASE_URL" >>= \case
        Just url -> pure $ T.pack url
        Nothing -> error "DATABASE_URL environment variable is not set"
