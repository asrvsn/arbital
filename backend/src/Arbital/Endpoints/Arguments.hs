{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Arbital.Endpoints.Arguments
  ( ArgumentsAPI
  , argumentsServer
  ) where

import Servant

import Arbital.Types
import Arbital.State

type ArgumentsAPI = 
       "arguments" :> Capture "argumentid" ArgumentID :> "items" :> Get '[JSON] [ClaimItem]

  :<|> "arguments" :> Capture "argumentid" ArgumentID :> Get '[JSON] Argument

argumentsServer :: ServerT ArgumentsAPI App
argumentsServer = 
  getArgumentClaims :<|> getArgument

getArgumentClaims :: ArgumentID -> App [ClaimItem]
getArgumentClaims = undefined

getArgument :: ArgumentID -> App Argument
getArgument = undefined