{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (void)
import Data.Int (Int32)
import Data.Text qualified as T
import Effectful
import Effectful.Exception
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
    , testCase "test connection stats retrieval with new connection" testConnectionStatsWithNewConnection
    ]

testGetLastQuery :: Assertion
testGetLastQuery = do
  dbUrl <- getConnString
  let connectionSource = simpleSource $ defaultConnectionSettings {csConnInfo = dbUrl}
  void . runEff . runDB (unConnectionSource connectionSource) defaultTransactionSettings $ do
    do
      -- Run the first query and perform some basic sanity checks
      let sql = "SELECT 1"
      rowNo <- runSQL sql
      liftIO $ assertEqual "One row should be retrieved" 1 rowNo
      result <- fetchMany (runIdentity @Int32)
      liftIO $ assertEqual "Result should be [1]" [1] result
      (_, SomeSQL lastQuery) <- getLastQuery
      liftIO $ assertEqual "SQL don't match" (show sql) (show lastQuery)
    do
      -- Run the second query and check that `getLastQuery` gives updated result
      let newSQL = "SELECT 2"
      runSQL_ newSQL
      (_, SomeSQL newLastQuery) <- getLastQuery
      liftIO $ assertEqual "SQL don't match" (show newSQL) (show newLastQuery)

testWithFrozenLastQuery :: Assertion
testWithFrozenLastQuery = do
  dbUrl <- getConnString
  let connectionSource = simpleSource $ defaultConnectionSettings {csConnInfo = dbUrl}
  void . runEff . runDB (unConnectionSource connectionSource) defaultTransactionSettings $ do
    let sql = "SELECT 1"
    runSQL_ sql
    withFrozenLastQuery $ do
      runSQL_ "SELECT 2"
      (_, SomeSQL lastQuery) <- getLastQuery
      liftIO $ assertEqual "The last query before freeze should be reported" (show sql) (show lastQuery)
    (_, SomeSQL lastQuery) <- getLastQuery
    liftIO $ assertEqual "The last query before freeze should be reported" (show sql) (show lastQuery)

testConnectionStatsWithNewConnection :: Assertion
testConnectionStatsWithNewConnection = do
  dbUrl <- getConnString
  let connectionSource = simpleSource $ defaultConnectionSettings {csConnInfo = dbUrl}
  void . runEff . runDB (unConnectionSource connectionSource) defaultTransactionSettings $ do
    runSQL_ "SELECT 1"
    runSQL_ "SELECT 2"
    stats <- getConnectionStats
    liftIO $ assertEqual "Incorrect statsQueries" 2 $ statsQueries stats
    unsafeWithoutTransaction
      . bracket_
        (runSQL_ "CREATE TABLE some_table (field INT)")
        (runSQL_ "DROP TABLE some_table")
      $ do
        runSQL_ "BEGIN"
        runSQL_ "INSERT INTO some_table VALUES (1)"
        withNewConnection $ do
          newStats <- getConnectionStats
          liftIO $ assertEqual "Connection stats should be reset" 0 $ statsQueries newStats
          noOfResults <- runSQL "SELECT * FROM some_table"
          liftIO $ assertEqual "Results should not be visible yet" 0 noOfResults
        runSQL_ "COMMIT"
        noOfResults <- runSQL "SELECT * FROM some_table"
        liftIO $ assertEqual "Results should be visible" 1 noOfResults

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
