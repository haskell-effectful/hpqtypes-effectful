{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main)
where

import Control.Monad.Base (liftBase)
import Data.Int (Int32)

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import MyLib
import System.Environment (getEnv)

import qualified Data.Text as T
import qualified Database.PostgreSQL.PQTypes as PQ

main :: IO ()
main = do
  dbUrl <- T.pack <$> getEnv "DATABASE_URL"
  let connectionSource = PQ.unConnectionSource $ PQ.simpleSource $ PQ.ConnectionSettings dbUrl Nothing []
      transactionSettings = PQ.defaultTransactionSettings
      sql :: PQ.SQL = PQ.mkSQL "SELECT 1"
      program :: Eff '[EffectDB, Error PQ.HPQTypesError, IOE] ()
      program = do
        rowNo <- runQuery sql
        liftBase $ putStr "Row number: " >> print rowNo
        queryResult :: [Int32] <- fetchMany PQ.runIdentity
        liftBase $ putStr "Result(s): " >> print queryResult
        connectionStats <- send $ GetConnectionStats
        liftBase $ putStr "Connection stats: " >> print connectionStats
  (runEff . runErrorNoCallStack @PQ.HPQTypesError $ runEffectDB connectionSource transactionSettings program) >>= print
