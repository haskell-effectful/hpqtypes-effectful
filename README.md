# hpqtypes-effectful

## Description

`effectful` bindings for the `hpqtypes` haskell bindings for `libpqtypes`

## How to use

This library provides a `DB` effect that allows the use of the hpqtypes bindings for libpqtypes in the effectful ecosystem.

An `Eff es` stack that contains `DB` allows the use of all functions
with a `MonadDB` constraint.

example:
```haskell
exampleProgram :: Eff '[DB, IOE] ()
exampleProgram = do
  runQuery_ $ mkSQL "CREATE TABLE some_table (field INT)"
  runQuery_ $ mkSQL "INSERT INTO some_table VALUES (1)"
  noOfResults <- runQuery $ mkSQL "SELECT * FROM some_table"
  liftIO $ assertEqual "Should get one result" 1 noOfResults
  runQuery_ $ mkSQL "DROP TABLE some_table"
```
