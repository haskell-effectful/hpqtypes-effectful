{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
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

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local

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
  -- RunPreparedQuery :: IsSQL sql => PQ.QueryName -> sql -> EffectDB m Int
  -- GetLastQuery :: EffectDB m SomeSQL
  -- WithFrozenLastQuery :: m a -> EffectDB m a


type instance DispatchOf EffectDB = 'Dynamic


runEffectDB
  :: forall es a m. (IOE :> es)
  -- => ConnectionSourceM m
  -- -> TransactionSettings
  => PQ.DBState m
  -> Eff (EffectDB : es) a
  -> Eff es a
runEffectDB dbState0 =
  reinterpret (evalState dbState0) $ \_ -> \case
    RunQuery sql -> do
      dbState <- get
      (result, dbState') <- liftBase $ PQ.runQueryIO sql (dbState :: PQ.DBState m)
      put dbState'
      pure result
  -- WithFrozenLastQuery (action :: Eff localEs b) -> do
  --   -- localSeqUnliftIO env $ \unlift -> unlift action
  --   localSeqUnliftIO env $ \unlift -> (unlift action :: IO b)
  --   -- liftIO $ PQ.runDBT undefined undefined $ PQ.withFrozenLastQuery result


main :: IO ()
main = do
  let connectionSettings :: PQ.ConnectionSourceM IO = undefined
      transactionSettings :: PQ.TransactionSettings = undefined
  PQ.withConnection connectionSettings $ \conn -> do
    let dbState :: PQ.DBState IO = undefined
          -- PQ.DBState {
          --   dbConnection = conn
          -- , dbConnectionSource = cs
          -- , dbTransactionSettings = ts
          -- , dbLastQuery = SomeSQL (mempty::SQL)
          -- , dbRecordLastQuery = True
          -- , dbQueryResult = Nothing
          -- }
        sql1 :: PQ.SQL = PQ.mkSQL ""
        sql2 :: PQ.SQL = PQ.mkSQL ""
        program :: Eff '[EffectDB, IOE] ()
        program = do
          queryResult1 <- send $ RunQuery sql1
          liftBase $ print queryResult1
          queryResult2 <- send $ RunQuery sql2
          liftBase $ print queryResult2
    runEff $ runEffectDB dbState program
