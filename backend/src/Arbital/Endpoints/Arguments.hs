{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Arbital.Endpoints.Arguments
  ( ArgumentsAPI
  , argumentsServer
  ) where

import Servant
import Data.Text (Text)

import Arbital.Types
import Arbital.State
import Arbital.Database.Items
import Arbital.Database.Driver

type ArgumentsAPI = 
       "arguments" :> Capture "argumentid" ArgumentID :> "claims" :> Get '[JSON] [Claim]

  :<|> "arguments" :> Capture "argumentid" ArgumentID :> "page" :> Get '[JSON] ArgumentPage

  :<|> "arguments" :> Capture "argumentid" ArgumentID :> Get '[JSON] Argument

  :<|> "arguments" :> "search" :> Capture "query" Text :> Get '[JSON] [Argument] 

argumentsServer :: ServerT ArgumentsAPI App
argumentsServer = claims :<|> argPage :<|> arg :<|> searchArgs
  where
    claims i = 
      withDb $ getArgument i >>= (mapM getClaim . argumentClaims)
    argPage i = withDb $ do
      a <- getArgument i
      cs <- mapM getClaim (argumentClaims a)
      return $ ArgumentPage a cs
    arg i = withDb $ getArgument i
    searchArgs q = withDb $ search Proxy 5 (Field "text" q)

