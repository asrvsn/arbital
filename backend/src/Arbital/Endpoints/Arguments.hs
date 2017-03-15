{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Arbital.Endpoints.Arguments
  ( ArgumentsAPI
  , argumentsServer
  ) where

import Servant

import Control.Monad ((>=>))

import Arbital.Types
import Arbital.State
import Arbital.Database.Items

type ArgumentsAPI = 
       "arguments" :> Capture "argumentid" ArgumentID :> "items" :> Get '[JSON] [ClaimItem]

  :<|> "arguments" :> Capture "argumentid" ArgumentID :> Get '[JSON] Argument

argumentsServer :: ServerT ArgumentsAPI App
argumentsServer = claimItems :<|> arg
  where
    claimItems i = 
      withDb $ getArgument i >>= (mapM (getClaim >=> getClaimItem) . argumentClaims)
    arg i = withDb $ getArgument i

