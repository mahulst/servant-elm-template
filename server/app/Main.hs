{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import           Control.Monad.Logger (runStderrLoggingT)
import           Data.String.Conversions

import Servant (Proxy(..), serve)
import Api.Server (server)
import Api.Types (ApiWithAssets)


import           Data.Text
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Api.Models

apiWithAssets :: Proxy ApiWithAssets
apiWithAssets = Proxy

app :: ConnectionPool -> Application
app pool = serve apiWithAssets $ server pool


mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
    pool <- runStderrLoggingT $ do
        createSqlitePool (cs sqliteFile) 5

    runSqlPool (runMigration migrateAll) pool
    return $ app pool


main :: IO ()
main = do
  let port = 3000
  run port =<< mkApp "test.db"


