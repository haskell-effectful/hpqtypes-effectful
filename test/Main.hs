{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad (void)
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Catch (MonadMask)
import Data.Int (Int32)
import qualified Data.Text as T
import Database.PostgreSQL.PQTypes hiding (queryResult)
import Effectful
import Effectful.Error.Static
import Effectful.HPQTypes
import System.Environment (getEnv)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "tests"
    [testCase "test PrintConnectionStats" testPrintConnectionStats]

testPrintConnectionStats :: Assertion
testPrintConnectionStats = do
  dbUrl <- T.pack <$> getEnv "DATABASE_URL"
  let connectionSource :: ConnectionSource [MonadBase IO, MonadMask]
      connectionSource = simpleSource $ ConnectionSettings dbUrl Nothing []
      transactionSettings = defaultTransactionSettings
      sql = "SELECT 1"
      program :: Eff '[DB, Error HPQTypesError, IOE] ()
      program = do
        rowNo <- runQuery $ mkSQL sql
        liftBase $ putStr "Row number: " >> print rowNo

        queryResult :: [Int32] <- fetchMany runIdentity
        liftBase $ putStr "Result(s): " >> print queryResult

        (SomeSQL lq) <- getLastQuery
        withFrozenLastQuery $ do
          let newSQL = "SELECT 2"
          void . runQuery $ mkSQL newSQL
          (SomeSQL newLq) <- getLastQuery
          liftIO $ assertEqual "SQL don't match" (show newLq) (show $ mkSQL sql)
        liftIO $ assertEqual "SQL don't match" (show lq) (show $ mkSQL sql)

        connectionStats <- getConnectionStats
        liftIO $ putStr "Connection stats: " >> print connectionStats

        setTransactionSettings $ defaultTransactionSettings {tsIsolationLevel = ReadCommitted}
        void . runQuery $ mkSQL "CREATE TABLE some_table (field INT)"
        void . runQuery $ mkSQL "BEGIN"
        void . runQuery $ mkSQL "INSERT INTO some_table VALUES (1)"
        withNewConnection $ do
          newConnectionStats <- getConnectionStats
          liftIO $ putStr "New connection stats: " >> print newConnectionStats

          setTransactionSettings $ defaultTransactionSettings {tsIsolationLevel = ReadCommitted}
          noOfResults <- runQuery $ mkSQL "SELECT * FROM some_table"
          liftIO $ assertEqual "Results should not be visible yet" 0 noOfResults
        void . runQuery $ mkSQL "COMMIT"
        noOfResults <- runQuery $ mkSQL "SELECT * FROM some_table"
        liftIO $ assertEqual "Results should be visible" 1 noOfResults
        void . runQuery $ mkSQL "DROP TABLE some_table"
  (runEff . runErrorNoCallStack @HPQTypesError $ runEffectDB connectionSource transactionSettings program) >>= print
