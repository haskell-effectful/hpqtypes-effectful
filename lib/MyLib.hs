{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}


module MyLib
  ( EffectDB
  ) where


import Effectful
import Database.PostgreSQL.PQTypes.SQL.Class (IsSQL)
import Database.PostgreSQL.PQTypes (SomeSQL)
import Database.PostgreSQL.PQTypes.Internal.Connection (ConnectionStats)
import qualified Database.PostgreSQL.PQTypes.SQL.Class as PQ
import qualified Database.PostgreSQL.PQTypes.Class as PQ
import Database.PostgreSQL.PQTypes.Internal.Monad (runDBT)

import Database.PostgreSQL.PQTypes.Internal.QueryResult (QueryResult)

import Effectful.Dispatch.Dynamic



data EffectDB :: Effect where
  RunQuery :: IsSQL sql => sql -> EffectDB m Int
  -- RunPreparedQuery :: IsSQL sql => PQ.QueryName -> sql -> EffectDB m Int
  -- GetLastQuery :: EffectDB m SomeSQL
  WithFrozenLastQuery :: EffectDB m a -> EffectDB m a


type instance DispatchOf EffectDB = 'Dynamic


runEffectDB
  :: (IOE :> es)
  => Eff (EffectDB : es) a
  -> Eff es a
runEffectDB = interpret $ \_ -> \case
  RunQuery sql -> liftIO $
    runDBT undefined undefined (PQ.runQuery sql)
  WithFrozenLastQuery dbEff ->
    undefined -- PQ.withFrozenLastQuery dbEff
