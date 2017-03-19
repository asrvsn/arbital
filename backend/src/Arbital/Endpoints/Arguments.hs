{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Arbital.Endpoints.Arguments
  ( ArgumentsAPI
  , argumentsServer
  ) where

import Servant

import Arbital.Types
import Arbital.State
import Arbital.Database.Items

type ArgumentsAPI = 
       "arguments" :> Capture "argumentid" ArgumentID :> "claims" :> Get '[JSON] [Claim]

  :<|> "arguments" :> Capture "argumentid" ArgumentID :> Get '[JSON] Argument

argumentsServer :: ServerT ArgumentsAPI App
argumentsServer = claims :<|> arg
  where
    claims i = 
      withDb $ getArgument i >>= (mapM getClaim . argumentClaims)
    arg i = withDb $ getArgument i

