{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Effectful.HPQTypes
  ( EffectDB (..)
  , runEffectDB
  )
where

import Control.Concurrent.MVar (readMVar)
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Catch (MonadMask)
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.Internal.Connection (Connection (..), cdStats, withConnection)
import Database.PostgreSQL.PQTypes.Internal.Notification (getNotificationIO)
import Database.PostgreSQL.PQTypes.Internal.Query (runPreparedQueryIO, runQueryIO)
import Database.PostgreSQL.PQTypes.Internal.State (DBState (..))
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local

data EffectDB :: Effect where
  RunQuery :: IsSQL sql => sql -> EffectDB m Int
  GetQueryResult :: FromRow row => EffectDB m (Maybe (QueryResult row))
  ClearQueryResult :: EffectDB m ()
  GetConnectionStats :: EffectDB m ConnectionStats
  RunPreparedQuery :: IsSQL sql => QueryName -> sql -> EffectDB m Int
  GetLastQuery :: EffectDB m SomeSQL
  GetTransactionSettings :: EffectDB m TransactionSettings
  SetTransactionSettings :: TransactionSettings -> EffectDB m ()
  WithFrozenLastQuery :: m a -> EffectDB m a
  WithNewConnection :: m a -> EffectDB m a
  GetNotification :: Int -> EffectDB m (Maybe Notification)

type instance DispatchOf EffectDB = 'Dynamic

instance EffectDB :> es => MonadDB (Eff es) where
  runQuery = send . RunQuery
  getQueryResult = send GetQueryResult
  clearQueryResult = send ClearQueryResult
  getConnectionStats = send GetConnectionStats
  runPreparedQuery qn = send . RunPreparedQuery qn
  getLastQuery = send GetLastQuery
  getTransactionSettings = send GetTransactionSettings
  setTransactionSettings = send . SetTransactionSettings
  withFrozenLastQuery = send . WithFrozenLastQuery
  withNewConnection = send . WithNewConnection
  getNotification = send . GetNotification

runEffectDB ::
  forall es a.
  (IOE :> es, Error HPQTypesError :> es) =>
  ConnectionSource [MonadBase IO, MonadMask] ->
  TransactionSettings ->
  Eff (EffectDB : es) a ->
  Eff es a
runEffectDB connectionSource transactionSettings =
  reinterpret runWithState $ \env -> \case
    RunQuery sql -> do
      dbState <- get
      (result, dbState') <- liftBase $ runQueryIO sql (dbState :: DBState (Eff es))
      put dbState'
      pure result
    GetQueryResult -> do
      dbState :: DBState (Eff es) <- get
      pure $ dbQueryResult dbState
    ClearQueryResult ->
      modify $ \(st :: DBState (Eff es)) -> st {dbQueryResult = Nothing}
    WithFrozenLastQuery (action :: Eff localEs b) -> do
      st :: DBState (Eff es) <- get
      put st {dbRecordLastQuery = False}
      result <- localSeqUnlift env $ \unlift -> unlift action
      modify $ \(st' :: DBState (Eff es)) ->
        st' {dbRecordLastQuery = dbRecordLastQuery st}
      pure result
    GetConnectionStats -> do
      dbState :: DBState (Eff es) <- get
      mconn <- liftIO . readMVar . unConnection $ dbConnection dbState
      case mconn of
        Nothing -> throwError $ HPQTypesError "getConnectionStats: no connection"
        Just cd -> return $ cdStats cd
    RunPreparedQuery queryName sql -> do
      dbState <- get
      (result, dbState') <- liftBase $ runPreparedQueryIO queryName sql (dbState :: DBState (Eff es))
      put dbState'
      pure result
    GetLastQuery -> do
      dbState :: DBState (Eff es) <- get
      pure $ dbLastQuery dbState
    GetTransactionSettings -> do
      dbState :: DBState (Eff es) <- get
      pure $ dbTransactionSettings dbState
    SetTransactionSettings settings -> modify $ \(st' :: DBState (Eff es)) ->
      st' {dbTransactionSettings = settings}
    WithNewConnection (action :: Eff localEs b) -> do
      runWithNewConnection $ localSeqUnlift env (\unlift -> unlift action)
    GetNotification time -> do
      dbState :: DBState (Eff es) <- get
      liftBase $ getNotificationIO dbState time
  where
    runWithState :: Eff (State (DBState (Eff es)) : es) a -> Eff es a
    runWithState eff =
      withConnection (unConnectionSource connectionSource) $ \conn -> do
        let dbState0 = mkDBState conn transactionSettings
        evalState dbState0 eff :: Eff es a
    runWithNewConnection ::
      Eff (State (DBState (Eff es)) : es) b ->
      Eff (State (DBState (Eff es)) : es) b
    runWithNewConnection action = do
      dbState :: DBState (Eff es) <- get
      result <- withConnection (unConnectionSource connectionSource) $ \newConn -> do
        -- TODO: We do not pass the current connection source to the new DB
        -- state, which differs from the original code.
        put $ mkDBState newConn (dbTransactionSettings dbState)
        action
      put dbState
      pure result
    mkDBState :: Connection -> TransactionSettings -> DBState (Eff es)
    mkDBState conn ts =
      DBState
        { dbConnection = conn
        , dbConnectionSource = unConnectionSource connectionSource
        , dbTransactionSettings = ts
        , dbLastQuery = SomeSQL (mempty :: SQL)
        , dbRecordLastQuery = True
        , dbQueryResult = Nothing
        }
