{-# LANGUAGE OverloadedStrings #-}

module Api.Example.Handler
    ( rollDice
    ) where

import Servant (Handler)        
import Control.Monad.IO.Class (liftIO)
import System.Random (getStdRandom, randomR)
import Api.Example.Types (Dice(..))
import Api.Models

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite


rollDice :: Dice -> Handler Int
rollDice dice = liftIO $ getStdRandom (randomR (1,(numSides dice)))


