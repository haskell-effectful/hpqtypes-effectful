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
  , runDB
  )
where

import Control.Concurrent.MVar (readMVar)
import Control.Monad.Catch
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
runDB ::
  forall es a.
  (IOE :> es, Error PQ.HPQTypesError :> es) =>
  PQ.ConnectionSourceM (Eff es) ->
  PQ.TransactionSettings ->
  Eff (DB : es) a ->
  Eff es a
runDB connectionSource transactionSettings =
  reinterpret runWithState $ \env -> \case
    RunQuery sql -> unDBEff $ PQ.runQuery sql
    GetQueryResult -> unDBEff PQ.getQueryResult
    ClearQueryResult -> unDBEff PQ.clearQueryResult
    GetConnectionStats -> unDBEff PQ.getConnectionStats
    RunPreparedQuery queryName sql -> unDBEff $ PQ.runPreparedQuery queryName sql
    GetLastQuery -> unDBEff PQ.getLastQuery
    GetTransactionSettings -> unDBEff PQ.getTransactionSettings
    SetTransactionSettings settings -> unDBEff $ PQ.setTransactionSettings settings
    WithFrozenLastQuery (action :: Eff localEs b) -> do
      localSeqUnlift env $ \unlift -> do
        unDBEff . PQ.withFrozenLastQuery . DBEff $ unlift action
    WithNewConnection (action :: Eff localEs b) -> do
      localSeqUnlift env $ \unlift -> do
        unDBEff . PQ.withNewConnection . DBEff $ unlift action
    GetNotification time -> unDBEff $ PQ.getNotification time
  where
    runWithState :: Eff (State (DBState es) : es) a -> Eff es a
    runWithState eff =
      PQ.withConnection connectionSource $ \conn -> do
        let dbState0 = mkDBState connectionSource conn transactionSettings
        evalState dbState0 $ handleAutoTransaction transactionSettings withTransaction' eff
    withTransaction' ::
      PQ.TransactionSettings ->
      Eff (State (DBState es) : es) a ->
      Eff (State (DBState es) : es) a
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
  -- We don't set tsAutoTransaction to False in the context of the action
  -- because if the action calls commit inside, then with tsAutoTransaction
  -- another transaction should be started automatically and if it's not set, it
  -- won't happen (see source of the commit' function).  On the other hand,
  -- withTransaction itself uses commit' and there we don't want to start
  -- another transaction.
  if PQ.tsAutoTransaction transactionSettings
    then withTransaction (transactionSettings {PQ.tsAutoTransaction = False}) action
    else action

---------------------------------------------------
-- Internal effect stack
---------------------------------------------------

-- | Newtype wrapper over the internal DB effect stack
newtype DBEff es a = DBEff
  { unDBEff :: Eff (State (DBState es) : es) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadMask)

-- | Internal state used to reinterpret the `DB` effect
type DBState es = PQ.DBState (Eff es)

-- Convenience `MonadIO` instance
instance (IOE :> es) => MonadIO (DBEff es) where
  liftIO b = DBEff $ liftIO b

get :: DBEff es (DBState es)
get = DBEff State.get

put :: DBState es -> DBEff es ()
put = DBEff . State.put

modify :: (DBState es -> DBState es) -> DBEff es ()
modify = DBEff . State.modify

instance (IOE :> es, Error PQ.HPQTypesError :> es) => PQ.MonadDB (DBEff es) where
  runQuery sql = do
    dbState <- get
    (result, dbState') <- liftIO $ PQ.runQueryIO sql dbState
    put dbState'
    pure result

  getQueryResult =
    get >>= \dbState -> pure $ PQ.dbQueryResult dbState

  clearQueryResult =
    modify $ \st -> st {PQ.dbQueryResult = Nothing}

  getConnectionStats = do
    dbState <- get
    mconn <- liftIO . readMVar . PQ.unConnection $ PQ.dbConnection dbState
    case mconn of
      Nothing -> DBEff . throwError $ PQ.HPQTypesError "getConnectionStats: no connection"
      Just cd -> pure $ PQ.cdStats cd

  runPreparedQuery queryName sql = do
    dbState <- get
    (result, dbState') <- liftIO $ PQ.runPreparedQueryIO queryName sql dbState
    put dbState'
    pure result

  getLastQuery = PQ.dbLastQuery <$> get

  getTransactionSettings = PQ.dbTransactionSettings <$> get

  setTransactionSettings settings = modify $ \st' ->
    st' {PQ.dbTransactionSettings = settings}

  withFrozenLastQuery action = do
    let restoreRecordLastQuery st =
          modify $ \st' ->
            st' {PQ.dbRecordLastQuery = PQ.dbRecordLastQuery st}
    bracket get restoreRecordLastQuery $ \st -> do
      put st {PQ.dbRecordLastQuery = False}
      action

  withNewConnection action = DBEff $ do
    dbState0 <- State.get
    raiseWith SeqUnlift $ \lower -> do
      PQ.withConnection (PQ.dbConnectionSource dbState0) $ \newConn -> lower $ do
        let transactionSettings = PQ.dbTransactionSettings dbState0
            dbState = mkDBState (PQ.dbConnectionSource dbState0) newConn transactionSettings
        unDBEff . bracket_ (put dbState) (put dbState0) $ do
          handleAutoTransaction transactionSettings PQ.withTransaction' action

  getNotification time = do
    dbState <- get
    liftIO $ PQ.getNotificationIO dbState time
