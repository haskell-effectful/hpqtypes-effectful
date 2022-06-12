{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}


module MyLib
  ( EffectDB
  ) where


import Control.Monad.Base (liftBase)
import Data.Int (Int32)
import qualified Data.Foldable as F

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Effectful.Error.Static
import Control.Concurrent.MVar
import System.Environment (getEnv)

import qualified Data.Text as T
import qualified Database.PostgreSQL.PQTypes as PQ
import qualified Database.PostgreSQL.PQTypes.Class as PQ
import qualified Database.PostgreSQL.PQTypes.SQL as PQ
import qualified Database.PostgreSQL.PQTypes.SQL.Class as PQ
import qualified Database.PostgreSQL.PQTypes.Internal.Connection as PQ
-- import qualified Database.PostgreSQL.PQTypes.Internal.Monad as PQ
import qualified Database.PostgreSQL.PQTypes.Internal.State as PQ
import qualified Database.PostgreSQL.PQTypes.Internal.Query as PQ
-- import qualified Database.PostgreSQL.PQTypes.Internal.QueryResult as PQ
import qualified Database.PostgreSQL.PQTypes.Transaction.Settings as PQ



data EffectDB :: Effect where
  RunQuery :: PQ.IsSQL sql => sql -> EffectDB m Int
  GetQueryResult :: PQ.FromRow row => EffectDB m (Maybe (PQ.QueryResult row))
  GetConnectionStats :: EffectDB m PQ.ConnectionStats
  -- GetQueryResult :: EffectDB m Bool
  -- RunPreparedQuery :: IsSQL sql => PQ.QueryName -> sql -> EffectDB m Int
  -- GetLastQuery :: EffectDB m SomeSQL
  -- WithFrozenLastQuery :: m a -> EffectDB m a


type instance DispatchOf EffectDB = 'Dynamic


{-# INLINABLE foldrDB #-}
foldrDB :: (PQ.FromRow row, EffectDB :> es) => (row -> acc -> Eff es acc) -> acc -> Eff es acc
foldrDB f acc = maybe (return acc) (F.foldrM f acc) =<< send GetQueryResult


{-# INLINABLE foldlDB #-}
foldlDB :: (PQ.FromRow row, EffectDB :> es) => (acc -> row -> Eff es acc) -> acc -> Eff es acc
foldlDB f acc = maybe (return acc) (F.foldlM f acc) =<< send GetQueryResult


{-# INLINABLE fetchMany #-}
fetchMany :: (PQ.FromRow row, EffectDB :> es) => (row -> t) -> Eff es [t]
fetchMany f = foldrDB (\row acc -> return $ f row : acc) []


runEffectDB
  :: forall es a. (IOE :> es, Error PQ.HPQTypesError :> es)
  => PQ.ConnectionSourceM (Eff es)
  -> PQ.TransactionSettings
  -> Eff (EffectDB : es) a
  -> Eff es a
runEffectDB connectionSource transactionSettings =
    reinterpret runWithState $ \_ -> \case
      RunQuery sql -> do
        dbState <- get
        (result, dbState') <- liftBase $ PQ.runQueryIO sql (dbState :: PQ.DBState (Eff es))
        put dbState'
        pure result
      GetQueryResult -> do
        dbState :: PQ.DBState (Eff es) <- get
        pure $ PQ.dbQueryResult dbState
      GetConnectionStats -> do
        dbState :: PQ.DBState (Eff es) <- get
        mconn <- liftIO . readMVar . PQ.unConnection $ PQ.dbConnection dbState 
        case mconn of
          Nothing -> throwError $ PQ.HPQTypesError "getConnectionStats: no connection"
          Just cd -> return $ PQ.cdStats cd

    -- WithFrozenLastQuery (action :: Eff localEs b) -> do
    --   -- localSeqUnliftIO env $ \unlift -> unlift action
    --   localSeqUnliftIO env $ \unlift -> (unlift action :: IO b)
    --   -- liftIO $ PQ.runDBT undefined undefined $ PQ.withFrozenLastQuery result
  where
    runWithState :: (IOE :> es) => Eff (State (PQ.DBState (Eff es)) : es) a -> Eff es a
    runWithState eff =
      PQ.withConnection connectionSource $ \conn -> do
        let dbState0 = mkDBConn conn
        evalState dbState0 eff :: Eff es a
    mkDBConn conn = PQ.DBState
      { PQ.dbConnection = conn
      , PQ.dbConnectionSource = connectionSource
      , PQ.dbTransactionSettings = transactionSettings
      , PQ.dbLastQuery = PQ.SomeSQL (mempty :: PQ.SQL)
      , PQ.dbRecordLastQuery = True
      , PQ.dbQueryResult = Nothing
      }


main :: IO ()
main = do
  dbUrl <- T.pack <$> getEnv "DATABASE_URL"
  let connectionSource = PQ.unConnectionSource $ PQ.simpleSource $ PQ.ConnectionSettings dbUrl Nothing []
      transactionSettings = PQ.defaultTransactionSettings
      sql :: PQ.SQL = PQ.mkSQL "SELECT 1"
      program :: Eff '[EffectDB, Error PQ.HPQTypesError, IOE] ()
      program = do
        rowNo <- send $ RunQuery sql
        liftBase $ putStr "Row number: " >> print rowNo
        queryResult :: [Int32] <- fetchMany PQ.runIdentity
        liftBase $ putStr "Result(s): " >> print queryResult
        connectionStats <- send $ GetConnectionStats
        liftBase $ putStr "Connection stats: " >> print connectionStats
  (runEff . runErrorNoCallStack @PQ.HPQTypesError $ runEffectDB connectionSource transactionSettings program) >>= print
