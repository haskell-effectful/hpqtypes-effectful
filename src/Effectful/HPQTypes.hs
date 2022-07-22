{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Effectful.HPQTypes
  ( DB (..)
  , runEffectDB
  )
where

import Control.Concurrent.MVar (readMVar)
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.State.Class (MonadState, get, modify, put, state)
import qualified Database.PostgreSQL.PQTypes as PQ
import qualified Database.PostgreSQL.PQTypes.Internal.Connection as PQ
import qualified Database.PostgreSQL.PQTypes.Internal.Notification as PQ
import qualified Database.PostgreSQL.PQTypes.Internal.Query as PQ
import qualified Database.PostgreSQL.PQTypes.Internal.State as PQ
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local (State, evalState)
import qualified Effectful.State.Static.Local as State

-- | An effect that allows the use of the hpqtypes bindings for libpqtypes in the effectful ecosystem.
--
-- An `Eff es` stack that contains `DB` allows the use of all functions
-- with a `MonadDB` constraint.
data DB :: Effect where
  RunQuery :: PQ.IsSQL sql => sql -> DB m Int
  GetQueryResult :: PQ.FromRow row => DB m (Maybe (PQ.QueryResult row))
  ClearQueryResult :: DB m ()
  GetConnectionStats :: DB m PQ.ConnectionStats
  RunPreparedQuery :: PQ.IsSQL sql => PQ.QueryName -> sql -> DB m Int
  GetLastQuery :: DB m PQ.SomeSQL
  GetTransactionSettings :: DB m PQ.TransactionSettings
  SetTransactionSettings :: PQ.TransactionSettings -> DB m ()
  WithFrozenLastQuery :: m a -> DB m a
  WithNewConnection :: m a -> DB m a
  GetNotification :: Int -> DB m (Maybe PQ.Notification)

type instance DispatchOf DB = 'Dynamic

-- This instance is not necessary to make the adapter work, but it can be useful
-- for library users.
instance DB :> es => PQ.MonadDB (Eff es) where
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
  Eff (DB : es) a ->
  Eff es a
runEffectDB connectionSource transactionSettings =
  reinterpret runWithState $ \env -> \case
    RunQuery sql -> unDBEff $ PQ.runQuery sql
    GetQueryResult -> unDBEff PQ.getQueryResult
    ClearQueryResult -> unDBEff PQ.clearQueryResult
    GetConnectionStats -> unDBEff PQ.getConnectionStats
    RunPreparedQuery queryName sql -> unDBEff $ PQ.runPreparedQuery queryName sql
    GetLastQuery -> unDBEff PQ.getLastQuery
    GetTransactionSettings -> unDBEff PQ.getTransactionSettings
    SetTransactionSettings settings -> unDBEff $ PQ.setTransactionSettings settings
    WithFrozenLastQuery (action :: Eff localEs b) ->
      unDBEff . PQ.withFrozenLastQuery . DBEff $
        localSeqUnlift env $
          \unlift -> unlift action
    WithNewConnection (action :: Eff localEs b) ->
      unDBEff . PQ.withNewConnection . DBEff $
        localSeqUnlift env $
          \unlift -> unlift action
    GetNotification time -> unDBEff $ PQ.getNotification time
  where
    runWithState :: Eff (DBState es : es) a -> Eff es a
    runWithState eff =
      PQ.withConnection (PQ.unConnectionSource connectionSource) $ \conn -> do
        let dbState0 = mkDBState (PQ.unConnectionSource connectionSource) conn transactionSettings
        evalState dbState0 $ handleAutoTransaction transactionSettings withTransaction' eff
    withTransaction' ::
      PQ.TransactionSettings ->
      Eff (DBState es : es) a ->
      Eff (DBState es : es) a
    withTransaction' ts eff = unDBEff . PQ.withTransaction' ts $ DBEff eff

mkDBState ::
  PQ.ConnectionSourceM m ->
  PQ.Connection ->
  PQ.TransactionSettings ->
  PQ.DBState m
mkDBState connectionSource conn ts =
  PQ.DBState
    { PQ.dbConnection = conn
    , PQ.dbConnectionSource = connectionSource
    , PQ.dbTransactionSettings = ts
    , PQ.dbLastQuery = PQ.SomeSQL (mempty :: PQ.SQL)
    , PQ.dbRecordLastQuery = True
    , PQ.dbQueryResult = Nothing
    }

handleAutoTransaction ::
  PQ.TransactionSettings ->
  (PQ.TransactionSettings -> m a -> m a) ->
  m a ->
  m a
handleAutoTransaction transactionSettings withTransaction action =
  -- TODO NOW: Why don't we have to set `tsAutoTransaction` to `False` in the
  -- context of the `action`?
  if PQ.tsAutoTransaction transactionSettings
    then withTransaction (transactionSettings {PQ.tsAutoTransaction = False}) action
    else action

---------------------------------------------------
-- Internal effect stack
---------------------------------------------------

-- | Newtype wrapper over the internal DB effect stack
newtype DBEff es a = DBEff
  { unDBEff :: Eff (DBState es : es) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadMask)

-- | Internal state effect used to reinterpret the `DB` effect
type DBState es = State (PQ.DBState (DBEff es))

-- Convenience @MonadBase IO@ instance
instance (IOE :> es) => MonadBase IO (DBEff es) where
  liftBase b = DBEff $ liftBase b

-- Convenience instance to avoid writing @DBEff get@ etc.
-- REVIEW: This comes with the mtl dependency, so maybe it isn't worth it?  The
-- `get`, `put`, and `modify` gelper functions could be also defined
-- explicitely.
instance MonadState (PQ.DBState (DBEff es)) (DBEff es) where
  get = DBEff State.get
  put = DBEff . State.put
  state = DBEff . State.state

instance (IOE :> es, Error PQ.HPQTypesError :> es) => PQ.MonadDB (DBEff es) where
  runQuery sql = do
    dbState <- get
    (result, dbState') <- liftBase $ PQ.runQueryIO sql dbState
    put dbState'
    pure result
  getQueryResult = PQ.dbQueryResult <$> get
  clearQueryResult =
    modify $ \st -> st {PQ.dbQueryResult = Nothing}
  getConnectionStats = do
    dbState <- get
    mconn <- liftBase . readMVar . PQ.unConnection $ PQ.dbConnection dbState
    case mconn of
      Nothing -> DBEff . throwError $ PQ.HPQTypesError "getConnectionStats: no connection"
      Just cd -> pure $ PQ.cdStats cd
  runPreparedQuery queryName sql = do
    dbState <- get
    (result, dbState') <- liftBase $ PQ.runPreparedQueryIO queryName sql dbState
    put dbState'
    pure result
  getLastQuery = PQ.dbLastQuery <$> get
  getTransactionSettings = PQ.dbTransactionSettings <$> get
  setTransactionSettings settings = modify $ \st' ->
    st' {PQ.dbTransactionSettings = settings}
  withFrozenLastQuery action = do
    st <- get
    put st {PQ.dbRecordLastQuery = False}
    result <- action
    modify $ \st' ->
      st' {PQ.dbRecordLastQuery = PQ.dbRecordLastQuery st}
    pure result
  withNewConnection action = do
    dbState <- get
    result <- PQ.withConnection (PQ.dbConnectionSource dbState) $ \newConn -> do
      let transactionSettings = PQ.dbTransactionSettings dbState
      put $ mkDBState (PQ.dbConnectionSource dbState) newConn transactionSettings
      handleAutoTransaction transactionSettings PQ.withTransaction' action
    put dbState
    pure result
  getNotification time = do
    dbState <- get
    liftBase $ PQ.getNotificationIO dbState time
