# hpqtypes-effectful

## Description

`effectful` bindings for the `hpqtypes` haskell bindings for `libpqtypes`

## How to use

This library exposes the `EffectDB` type-level effect that you can declare in your type signatures.

An `Eff es` stack that contains `EffectDB` can use all functions
that have a `MonadDB` constraint.

example:
```haskell
exampleProgram :: Eff '[EffectDB, Error HPQTypesError, IOE] ()
exampleProgram = do
  runQuery_ $ mkSQL "CREATE TABLE some_table (field INT)"
  runQuery_ $ mkSQL "INSERT INTO some_table VALUES (1)"
  noOfResults <- runQuery $ mkSQL "SELECT * FROM some_table"
  liftIO $ assertEqual "Should get one result" 1 noOfResults
  runQuery_ $ mkSQL "DROP TABLE some_table"
```
