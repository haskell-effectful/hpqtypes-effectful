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
import qualified Database.PostgreSQL.PQTypes as PQ
import Database.PostgreSQL.PQTypes.SQL.Class
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
  let connectionSource :: PQ.ConnectionSource [MonadBase IO, MonadMask]
      connectionSource = PQ.simpleSource $ PQ.ConnectionSettings dbUrl Nothing []
      transactionSettings = PQ.defaultTransactionSettings
      sql = "SELECT 1"
      program :: Eff '[EffectDB, Error PQ.HPQTypesError, IOE] ()
      program = do
        rowNo <- runQuery $ PQ.mkSQL sql
        liftBase $ putStr "Row number: " >> print rowNo

        queryResult :: [Int32] <- fetchMany PQ.runIdentity
        liftBase $ putStr "Result(s): " >> print queryResult

        connectionStats <- getConnectionStats
        liftBase $ putStr "Connection stats: " >> print connectionStats

        (SomeSQL lq) <- getLastQuery
        withFrozenLastQuery $ do
          let newSQL = "SELECT 2"
          void . runQuery $ PQ.mkSQL newSQL
          (SomeSQL newLq) <- getLastQuery
          liftIO $ assertEqual "SQL don't match" (show newLq) (show $ PQ.mkSQL sql)
        liftIO $ assertEqual "SQL don't match" (show lq) (show $ PQ.mkSQL sql)
  (runEff . runErrorNoCallStack @PQ.HPQTypesError $ runEffectDB connectionSource transactionSettings program) >>= print
