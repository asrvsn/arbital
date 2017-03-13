{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Arbital.Endpoints 
  ( app
  ) where

import Servant

import Arbital.Types
import Arbital.Endpoints.Arguments
import Arbital.Endpoints.Claims
import Arbital.Endpoints.Users

-- GET to / to get a page of all claims
type RootAPI = Get '[JSON] [ClaimItem]

rootServer :: Server RootAPI
rootServer = getAllClaimItems

type ServerAPI = RootAPI 
            :<|> ArgumentsAPI
            :<|> ClaimsAPI
            :<|> UsersAPI

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

app :: Application
app = serve serverAPI server

server :: Server ServerAPI
server = 
        rootServer
  :<|>  argumentsServer
  :<|>  claimsServer
  :<|>  usersServer

