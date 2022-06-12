{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}


module MyLib
  ( EffectDB
  ) where


import Effectful
import Database.PostgreSQL.PQTypes.SQL.Class (IsSQL)
import Database.PostgreSQL.PQTypes (SomeSQL)
import Database.PostgreSQL.PQTypes.Internal.Connection (ConnectionStats)
import qualified Database.PostgreSQL.PQTypes.SQL.Class as PQ
import qualified Database.PostgreSQL.PQTypes.Class as PQ
import Database.PostgreSQL.PQTypes.Internal.Monad (runDBT, DBT)

import Database.PostgreSQL.PQTypes.Internal.QueryResult (QueryResult)

import Effectful.Dispatch.Dynamic


-- data DBE :: Effect where
--   DBE :: DBT IO a -> DBE m a


-- type instance DispatchOf DBE = 'Dynamic


-- runDBE :: (IOE :> es) => Eff (DBE : es) a -> Eff es a
-- runDBE = undefined


data EffectDB :: Effect where
  RunQuery :: IsSQL sql => sql -> EffectDB m Int
  -- RunPreparedQuery :: IsSQL sql => PQ.QueryName -> sql -> EffectDB m Int
  -- GetLastQuery :: EffectDB m SomeSQL
  WithFrozenLastQuery :: m a -> EffectDB m a


type instance DispatchOf EffectDB = 'Dynamic


runEffectDB
  :: forall es a. (IOE :> es)
  => Eff (EffectDB : es) a
  -> Eff es a
runEffectDB = interpret $ \env -> \case
  RunQuery sql -> liftIO .
    runDBT undefined undefined $ PQ.runQuery sql
  WithFrozenLastQuery (action :: Eff localEs b) -> do
    -- localSeqUnliftIO env $ \unlift -> unlift action
    localSeqUnliftIO env $ \unlift -> (unlift action :: IO b)
    -- liftIO $ runDBT undefined undefined $ PQ.withFrozenLastQuery result


-- runEffectDB
--   :: forall es a. (IOE :> es)
--   => Eff (EffectDB : es) a
--   -> Eff es a
-- runEffectDB = interpret $ \env -> \case
--   RunQuery sql -> send . DBE $ PQ.runQuery sql
--   WithFrozenLastQuery (action :: Eff localEs b) -> do
--     localSeqUnlift env $ \unlift -> PQ.withFrozenLastQuery (unlift action :: Eff es b)
--     -- liftIO $ runDBT undefined undefined $ PQ.withFrozenLastQuery result
