{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Effectful.HPQTypes
  ( DB (..)
  , runEffectDB
  )
where

import Control.Concurrent.MVar (readMVar)
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import qualified Control.Monad.State.Class as ST
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
    RunQuery sql -> unWithDBState $ PQ.runQuery sql
    GetQueryResult -> unWithDBState PQ.getQueryResult
    ClearQueryResult -> unWithDBState PQ.clearQueryResult
    GetConnectionStats -> unWithDBState PQ.getConnectionStats
    RunPreparedQuery queryName sql -> unWithDBState $ PQ.runPreparedQuery queryName sql
    GetLastQuery -> unWithDBState PQ.getLastQuery
    GetTransactionSettings -> unWithDBState PQ.getTransactionSettings
    SetTransactionSettings settings -> unWithDBState $ PQ.setTransactionSettings settings
    WithFrozenLastQuery (action :: Eff localEs b) ->
      unWithDBState . PQ.withFrozenLastQuery . WithDBState $
        localSeqUnlift env $ \unlift -> unlift action
    WithNewConnection (action :: Eff localEs b) ->
      unWithDBState . PQ.withNewConnection . WithDBState $
        localSeqUnlift env $ \unlift -> unlift action
    GetNotification time -> unWithDBState $ PQ.getNotification time
  where
    runWithState :: Eff (DBInternal es) a -> Eff es a
    runWithState eff =
      PQ.withConnection (PQ.unConnectionSource connectionSource) $ \conn -> do
        let dbState0 = mkDBState (PQ.unConnectionSource connectionSource) conn transactionSettings
            -- TODO NOW: Is it ok that we now look at transactionSettings?
            -- Probably yes, since at this point we have no access to any other
            -- settings.  However, make sure about that.
            eff' = if PQ.tsAutoTransaction transactionSettings
              then withTransaction' (transactionSettings { PQ.tsAutoTransaction = False }) eff
              else eff
        evalState dbState0 eff' :: Eff es a

    withTransaction'
      :: PQ.TransactionSettings
      -> Eff (DBInternal es) a
      -> Eff (DBInternal es) a
    withTransaction' ts eff = unWithDBState . PQ.withTransaction' ts $ WithDBState eff

-- TODO NOW: This instance is probably too general to be defined here
instance ST.MonadState s (Eff (State s : es)) where
  get = get
  put = put
  state = state

type DBInternal es = State (PQ.DBState (WithDBState es)) : es

newtype WithDBState es a = WithDBState
  { unWithDBState :: Eff (DBInternal es) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadMask)
  deriving newtype (ST.MonadState (PQ.DBState (WithDBState es)))

instance (IOE :> es) => MonadBase IO (WithDBState es) where
  liftBase b = WithDBState $ liftBase b

instance (IOE :> es, Error PQ.HPQTypesError :> es) => PQ.MonadDB (WithDBState es) where
  runQuery sql = do
    dbState <- ST.get
    (result, dbState') <- liftBase $ PQ.runQueryIO sql dbState
    ST.put dbState'
    pure result
  getQueryResult = PQ.dbQueryResult <$> ST.get
  clearQueryResult =
    ST.modify $ \st -> st {PQ.dbQueryResult = Nothing}
  getConnectionStats = do
    dbState <- ST.get
    mconn <- liftBase . readMVar . PQ.unConnection $ PQ.dbConnection dbState
    case mconn of
      Nothing -> WithDBState . throwError $ PQ.HPQTypesError "getConnectionStats: no connection"
      Just cd -> pure $ PQ.cdStats cd
  runPreparedQuery queryName sql = do
    dbState <- ST.get
    (result, dbState') <- liftBase $ PQ.runPreparedQueryIO queryName sql dbState
    ST.put dbState'
    pure result
  getLastQuery = PQ.dbLastQuery <$> ST.get
  getTransactionSettings = PQ.dbTransactionSettings <$> ST.get
  setTransactionSettings settings = ST.modify $ \st' ->
    st' {PQ.dbTransactionSettings = settings}
  withFrozenLastQuery action = do
    st <- ST.get
    ST.put st {PQ.dbRecordLastQuery = False}
    result <- action
    ST.modify $ \st' ->
      st' {PQ.dbRecordLastQuery = PQ.dbRecordLastQuery st}
    pure result
  withNewConnection action = do
    dbState <- ST.get
    -- TODO NOW: We now take connection source from the DB state, which differs
    -- from the previous implementation; is that better or worse?
    result <- PQ.withConnection (PQ.dbConnectionSource dbState) $ \newConn -> do
      -- TODO NOW: We do not pass the current connection source to the new DB
      -- state, which differs from the original code.
      ST.put $ mkDBState (PQ.dbConnectionSource dbState) newConn (PQ.dbTransactionSettings dbState)
      action
    ST.put dbState
    pure result
  getNotification time = do
    dbState <- ST.get
    liftBase $ PQ.getNotificationIO dbState time

mkDBState
  :: PQ.ConnectionSourceM m
  -> PQ.Connection
  -> PQ.TransactionSettings
  -> PQ.DBState m
mkDBState connectionSource conn ts =
  PQ.DBState
    { PQ.dbConnection = conn
    , PQ.dbConnectionSource = connectionSource
    , PQ.dbTransactionSettings = ts
    , PQ.dbLastQuery = PQ.SomeSQL (mempty :: PQ.SQL)
    , PQ.dbRecordLastQuery = True
    , PQ.dbQueryResult = Nothing
    }
