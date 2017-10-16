{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Api.Types
    ( Api
    , ApiWithAssets
    ) where

import Servant ((:<|>), (:>), Post, JSON, ReqBody, Raw)
import Api.Example.Types (Dice)
import Api.Models
type Api
        = "api"
            :> ("rollDice" :> ReqBody '[JSON] Dice
                           :> Post '[JSON] Int
               )
           :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe (Key User))

type ApiWithAssets = "servant-elm-template" :> (Api :<|> Raw)