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
import qualified Database.PostgreSQL.PQTypes as PQ
import qualified Database.PostgreSQL.PQTypes.Internal.Connection as PQ
import qualified Database.PostgreSQL.PQTypes.Internal.Notification as PQ
import qualified Database.PostgreSQL.PQTypes.Internal.Query as PQ
import qualified Database.PostgreSQL.PQTypes.Internal.State as PQ
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local

-- | An effect that allows the use of the hpqtypes bindings for libpqtypes in the effectful ecosystem.
--
-- An `Eff es` stack that contains `EffectDB` allows the use of all functions
-- with a `MonadDB` constraint.
data EffectDB :: Effect where
  RunQuery :: PQ.IsSQL sql => sql -> EffectDB m Int
  GetQueryResult :: PQ.FromRow row => EffectDB m (Maybe (PQ.QueryResult row))
  ClearQueryResult :: EffectDB m ()
  GetConnectionStats :: EffectDB m PQ.ConnectionStats
  RunPreparedQuery :: PQ.IsSQL sql => PQ.QueryName -> sql -> EffectDB m Int
  GetLastQuery :: EffectDB m PQ.SomeSQL
  GetTransactionSettings :: EffectDB m PQ.TransactionSettings
  SetTransactionSettings :: PQ.TransactionSettings -> EffectDB m ()
  WithFrozenLastQuery :: m a -> EffectDB m a
  WithNewConnection :: m a -> EffectDB m a
  GetNotification :: Int -> EffectDB m (Maybe PQ.Notification)

type instance DispatchOf EffectDB = 'Dynamic

instance EffectDB :> es => PQ.MonadDB (Eff es) where
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

-- | The default effect runner.
runEffectDB ::
  forall es a.
  (IOE :> es, Error PQ.HPQTypesError :> es) =>
  PQ.ConnectionSource [MonadBase IO, MonadMask] ->
  PQ.TransactionSettings ->
  Eff (EffectDB : es) a ->
  Eff es a
runEffectDB connectionSource transactionSettings =
  reinterpret runWithState $ \env -> \case
    RunQuery sql -> do
      dbState <- get
      (result, dbState') <- liftBase $ PQ.runQueryIO sql (dbState :: PQ.DBState (Eff es))
      put dbState'
      pure result
    GetQueryResult -> do
      dbState :: PQ.DBState (Eff es) <- get
      pure $ PQ.dbQueryResult dbState
    ClearQueryResult ->
      modify $ \(st :: PQ.DBState (Eff es)) -> st {PQ.dbQueryResult = Nothing}
    WithFrozenLastQuery (action :: Eff localEs b) -> do
      st :: PQ.DBState (Eff es) <- get
      put st {PQ.dbRecordLastQuery = False}
      result <- localSeqUnlift env $ \unlift -> unlift action
      modify $ \(st' :: PQ.DBState (Eff es)) ->
        st' {PQ.dbRecordLastQuery = PQ.dbRecordLastQuery st}
      pure result
    GetConnectionStats -> do
      dbState :: PQ.DBState (Eff es) <- get
      mconn <- liftIO . readMVar . PQ.unConnection $ PQ.dbConnection dbState
      case mconn of
        Nothing -> throwError $ PQ.HPQTypesError "getConnectionStats: no connection"
        Just cd -> return $ PQ.cdStats cd
    RunPreparedQuery queryName sql -> do
      dbState <- get
      (result, dbState') <- liftBase $ PQ.runPreparedQueryIO queryName sql (dbState :: PQ.DBState (Eff es))
      put dbState'
      pure result
    GetLastQuery -> do
      dbState :: PQ.DBState (Eff es) <- get
      pure $ PQ.dbLastQuery dbState
    GetTransactionSettings -> do
      dbState :: PQ.DBState (Eff es) <- get
      pure $ PQ.dbTransactionSettings dbState
    SetTransactionSettings settings -> modify $ \(st' :: PQ.DBState (Eff es)) ->
      st' {PQ.dbTransactionSettings = settings}
    WithNewConnection (action :: Eff localEs b) -> do
      runWithNewConnection $ localSeqUnlift env (\unlift -> unlift action)
    GetNotification time -> do
      dbState :: PQ.DBState (Eff es) <- get
      liftBase $ PQ.getNotificationIO dbState time
  where
    runWithState :: Eff (State (PQ.DBState (Eff es)) : es) a -> Eff es a
    runWithState eff =
      PQ.withConnection (PQ.unConnectionSource connectionSource) $ \conn -> do
        let dbState0 = mkDBState conn transactionSettings
        evalState dbState0 eff :: Eff es a
    runWithNewConnection ::
      Eff (State (PQ.DBState (Eff es)) : es) b ->
      Eff (State (PQ.DBState (Eff es)) : es) b
    runWithNewConnection action = do
      dbState :: PQ.DBState (Eff es) <- get
      result <- PQ.withConnection (PQ.unConnectionSource connectionSource) $ \newConn -> do
        -- TODO: We do not pass the current connection source to the new DB
        -- state, which differs from the original code.
        put $ mkDBState newConn (PQ.dbTransactionSettings dbState)
        action
      put dbState
      pure result
    mkDBState :: PQ.Connection -> PQ.TransactionSettings -> PQ.DBState (Eff es)
    mkDBState conn ts =
      PQ.DBState
        { PQ.dbConnection = conn
        , PQ.dbConnectionSource = PQ.unConnectionSource connectionSource
        , PQ.dbTransactionSettings = ts
        , PQ.dbLastQuery = PQ.SomeSQL (mempty :: PQ.SQL)
        , PQ.dbRecordLastQuery = True
        , PQ.dbQueryResult = Nothing
        }
