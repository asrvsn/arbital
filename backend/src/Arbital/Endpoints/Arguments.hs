{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Arbital.Endpoints.Arguments
  ( ArgumentsAPI
  , argumentsServer
  ) where

import Servant

import Arbital.Types

type ArgumentsAPI = 
       "arguments" :> Capture "argumentid" ArgumentID :> "items" :> Get '[JSON] [ClaimItem]

  :<|> "arguments" :> Capture "argumentid" ArgumentID :> Get '[JSON] Argument

argumentsServer :: Server ArgumentsAPI
argumentsServer = 
  getArgumentClaims :<|> getArgument

getArgumentClaims :: ArgumentID -> Handler [ClaimItem]
getArgumentClaims = undefined

getArgument :: ArgumentID -> Handler Argument
getArgument = undefined