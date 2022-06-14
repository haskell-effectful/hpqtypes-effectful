{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Effectful.HPQTypes
  ( EffectDB (..)
  , runQuery
  , getQueryResult
  , getConnectionStats
  , runPreparedQuery
  , getLastQuery
  , withFrozenLastQuery
  , getNotification
  , foldrDB
  , foldlDB
  , fetchMany
  , runEffectDB
  , getTransactionSettings
  , setTransactionSettings
  , withNewConnection
  )
where

import Control.Concurrent.MVar
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Catch (MonadMask)
import qualified Data.Foldable as F
import qualified Database.PostgreSQL.PQTypes as PQ
import qualified Database.PostgreSQL.PQTypes.Internal.Connection as PQ
import qualified Database.PostgreSQL.PQTypes.Internal.Notification as PQ
import qualified Database.PostgreSQL.PQTypes.Internal.Query as PQ
import qualified Database.PostgreSQL.PQTypes.Internal.State as PQ
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local

data EffectDB :: Effect where
  RunQuery :: PQ.IsSQL sql => sql -> EffectDB m Int
  GetQueryResult :: PQ.FromRow row => EffectDB m (Maybe (PQ.QueryResult row))
  GetConnectionStats :: EffectDB m PQ.ConnectionStats
  RunPreparedQuery :: PQ.IsSQL sql => PQ.QueryName -> sql -> EffectDB m Int
  GetLastQuery :: EffectDB m PQ.SomeSQL
  GetTransactionSettings :: EffectDB m PQ.TransactionSettings
  SetTransactionSettings :: PQ.TransactionSettings -> EffectDB m ()
  WithFrozenLastQuery :: m a -> EffectDB m a
  WithNewConnection :: m a -> EffectDB m a
  GetNotification :: Int -> EffectDB m (Maybe PQ.Notification)

type instance DispatchOf EffectDB = 'Dynamic

runQuery :: (EffectDB :> es, PQ.IsSQL sql) => sql -> Eff es Int
runQuery = send . RunQuery

getQueryResult :: (EffectDB :> es, PQ.FromRow row) => Eff es (Maybe (PQ.QueryResult row))
getQueryResult = send GetQueryResult

getConnectionStats :: EffectDB :> es => Eff es PQ.ConnectionStats
getConnectionStats = send GetConnectionStats

runPreparedQuery :: (EffectDB :> es, PQ.IsSQL sql) => PQ.QueryName -> sql -> Eff es Int
runPreparedQuery = send ... RunPreparedQuery
  where
    (...) = (.) . (.)

getLastQuery :: EffectDB :> es => Eff es PQ.SomeSQL
getLastQuery = send GetLastQuery

withFrozenLastQuery :: EffectDB :> es => Eff es a -> Eff es a
withFrozenLastQuery = send . WithFrozenLastQuery

setTransactionSettings :: EffectDB :> es => PQ.TransactionSettings -> Eff es ()
setTransactionSettings = send . SetTransactionSettings

getTransactionSettings :: EffectDB :> es => Eff es PQ.TransactionSettings
getTransactionSettings = send GetTransactionSettings

getNotification :: EffectDB :> es => Int -> Eff es (Maybe PQ.Notification)
getNotification = send . GetNotification

withNewConnection :: (EffectDB :> es) => Eff es a -> Eff es a
withNewConnection = send . WithNewConnection

{-# INLINEABLE foldrDB #-}
foldrDB :: (PQ.FromRow row, EffectDB :> es) => (row -> acc -> Eff es acc) -> acc -> Eff es acc
foldrDB f acc = maybe (return acc) (F.foldrM f acc) =<< getQueryResult

{-# INLINEABLE foldlDB #-}
foldlDB :: (PQ.FromRow row, EffectDB :> es) => (acc -> row -> Eff es acc) -> acc -> Eff es acc
foldlDB f acc = maybe (return acc) (F.foldlM f acc) =<< getQueryResult

{-# INLINEABLE fetchMany #-}
fetchMany :: (PQ.FromRow row, EffectDB :> es) => (row -> t) -> Eff es [t]
fetchMany f = foldrDB (\row acc -> return $ f row : acc) []

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
