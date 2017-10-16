{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Server
     ( server
     ) where

import Servant ((:<|>)((:<|>)), Server, serveDirectoryFileServer)
import Api.Types (ApiWithAssets)
import Api.Example.Handler (rollDice)
import           Control.Monad.IO.Class

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import Api.Models
server :: ConnectionPool -> Server ApiWithAssets
server pool = rollDice' :<|> userAddH :<|> serveStatic'
    where
        rollDice' = rollDice
        userAddH newUser = liftIO $ userAdd newUser

        userAdd :: User -> IO (Maybe (Key User))
        userAdd newUser = flip runSqlPersistMPool pool $ do
              exists <- selectFirst [UserName ==. (userName newUser)] []
              case exists of
                Nothing -> Just <$> insert newUser
                Just _ -> return Nothing

        serveStatic' = serveDirectoryFileServer "../client/dist"