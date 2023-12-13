{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Access to a PostgreSQL database via 'MonadDB'.
module Effectful.HPQTypes
  ( -- * Effect
    DB (..)

    -- ** Handlers
  , runDB

    -- * Re-exports
  , module Database.PostgreSQL.PQTypes
  )
where

import Control.Concurrent.MVar (readMVar)
import Control.Monad.Catch
import Database.PostgreSQL.PQTypes
import qualified Database.PostgreSQL.PQTypes.Internal.Connection as PQ
import qualified Database.PostgreSQL.PQTypes.Internal.Notification as PQ
import qualified Database.PostgreSQL.PQTypes.Internal.State as PQ
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local (State, evalState)
import qualified Effectful.State.Static.Local as State
import GHC.Stack

-- | Provide the ability to access a PostgreSQL database via 'MonadDB'.
data DB :: Effect where
  RunQuery :: IsSQL sql => sql -> DB m Int
  GetQueryResult :: FromRow row => DB m (Maybe (QueryResult row))
  ClearQueryResult :: DB m ()
  GetConnectionStats :: DB m PQ.ConnectionStats
  RunPreparedQuery :: IsSQL sql => PQ.QueryName -> sql -> DB m Int
  GetLastQuery :: DB m SomeSQL
  GetTransactionSettings :: DB m TransactionSettings
  SetTransactionSettings :: TransactionSettings -> DB m ()
  WithFrozenLastQuery :: m a -> DB m a
  WithNewConnection :: m a -> DB m a
  GetNotification :: Int -> DB m (Maybe PQ.Notification)

type instance DispatchOf DB = Dynamic

-- | Orphan, canonical instance.
instance DB :> es => MonadDB (Eff es) where
  runQuery = withFrozenCallStack $ send . RunQuery
  getQueryResult = send GetQueryResult
  clearQueryResult = send ClearQueryResult
  getConnectionStats = withFrozenCallStack $ send GetConnectionStats
  runPreparedQuery qn = withFrozenCallStack $ send . RunPreparedQuery qn
  getLastQuery = send GetLastQuery
  getTransactionSettings = send GetTransactionSettings
  setTransactionSettings = send . SetTransactionSettings
  withFrozenLastQuery = send . WithFrozenLastQuery
  withNewConnection = send . WithNewConnection
  getNotification = send . GetNotification

-- | Run the 'DB' effect with the given connection source and transaction
-- settings.
--
-- /Note:/ this is the @effectful@ version of 'runDBT'.
runDB
  :: forall es a
   . (IOE :> es)
  => PQ.ConnectionSourceM (Eff es)
  -- ^ Connection source.
  -> TransactionSettings
  -- ^ Transaction settings.
  -> Eff (DB : es) a
  -> Eff es a
runDB connectionSource transactionSettings =
  reinterpret runWithState $ \env -> \case
    RunQuery sql -> unDBEff $ runQuery sql
    GetQueryResult -> unDBEff getQueryResult
    ClearQueryResult -> unDBEff clearQueryResult
    GetConnectionStats -> unDBEff getConnectionStats
    RunPreparedQuery queryName sql -> unDBEff $ runPreparedQuery queryName sql
    GetLastQuery -> unDBEff getLastQuery
    GetTransactionSettings -> unDBEff getTransactionSettings
    SetTransactionSettings settings -> unDBEff $ setTransactionSettings settings
    WithFrozenLastQuery (action :: Eff localEs b) -> do
      localSeqUnlift env $ \unlift -> do
        unDBEff . withFrozenLastQuery . DBEff $ unlift action
    WithNewConnection (action :: Eff localEs b) -> do
      localSeqUnlift env $ \unlift -> do
        unDBEff . withNewConnection . DBEff $ unlift action
    GetNotification time -> unDBEff $ getNotification time
  where
    runWithState :: Eff (State (DBState es) : es) a -> Eff es a
    runWithState eff =
      PQ.withConnection connectionSource $ \conn -> do
        let dbState0 = mkDBState connectionSource conn transactionSettings
        evalState dbState0 $ do
          handleAutoTransaction transactionSettings doWithTransaction eff

    doWithTransaction
      :: TransactionSettings
      -> Eff (State (DBState es) : es) a
      -> Eff (State (DBState es) : es) a
    doWithTransaction ts eff = unDBEff . withTransaction' ts $ DBEff eff

mkDBState
  :: PQ.ConnectionSourceM m
  -> PQ.Connection
  -> TransactionSettings
  -> PQ.DBState m
mkDBState connectionSource conn ts =
  PQ.DBState
    { PQ.dbConnection = conn
    , PQ.dbConnectionSource = connectionSource
    , PQ.dbTransactionSettings = ts
    , PQ.dbLastQuery = SomeSQL (mempty :: SQL)
    , PQ.dbRecordLastQuery = True
    , PQ.dbQueryResult = Nothing
    }

handleAutoTransaction
  :: TransactionSettings
  -> (TransactionSettings -> m a -> m a)
  -> m a
  -> m a
handleAutoTransaction transactionSettings doWithTransaction action =
  -- We don't set tsAutoTransaction to False in the context of the action
  -- because if the action calls commit inside, then with tsAutoTransaction
  -- another transaction should be started automatically and if it's not set, it
  -- won't happen (see source of the commit' function).  On the other hand,
  -- withTransaction itself uses commit' and there we don't want to start
  -- another transaction.
  if tsAutoTransaction transactionSettings
    then doWithTransaction (transactionSettings {tsAutoTransaction = False}) action
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

instance (IOE :> es) => MonadDB (DBEff es) where
  runQuery sql = do
    dbState <- get
    (rows, res) <- liftIO $ PQ.runQueryIO (PQ.dbConnection dbState) sql
    put $ PQ.updateStateWith dbState sql res
    pure rows

  getQueryResult =
    get >>= \dbState -> pure $ PQ.dbQueryResult dbState

  clearQueryResult =
    modify $ \st -> st {PQ.dbQueryResult = Nothing}

  getConnectionStats = do
    dbState <- get
    mconn <- liftIO . readMVar . PQ.unConnection $ PQ.dbConnection dbState
    case mconn of
      Nothing -> throwDB $ HPQTypesError "getConnectionStats: no connection"
      Just cd -> pure $ PQ.cdStats cd

  runPreparedQuery queryName sql = do
    dbState <- get
    (rows, res) <- liftIO $ PQ.runPreparedQueryIO (PQ.dbConnection dbState) queryName sql
    put $ PQ.updateStateWith dbState sql res
    pure rows

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
          handleAutoTransaction transactionSettings withTransaction' action

  getNotification time = do
    dbState <- get
    liftIO $ PQ.getNotificationIO dbState time
