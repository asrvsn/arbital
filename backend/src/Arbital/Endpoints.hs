{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Arbital.Endpoints 
  ( app
  ) where

import Servant
import Servant.Server (serveWithContext)

import Arbital.Types
import Arbital.State
import Arbital.Endpoints.Arguments
import Arbital.Endpoints.Claims
import Arbital.Endpoints.Users
import Arbital.Endpoints.Authenticated

-- GET to / to get a page of all claims
type RootAPI = Get '[JSON] [Claim]

rootServer :: ServerT RootAPI App
rootServer = retrieveAllClaims

type PrivateAPI = RootAPI 
             :<|> ArgumentsAPI
             :<|> ClaimsAPI
             :<|> UsersAPI

type PublicAPI = "login" :> ReqBody '[JSON] AuthStrategy :> Post '[JSON] Session

type ServerAPI = 
        AuthProtect "cookie-auth" :> PrivateAPI
  :<|>  PublicAPI

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

app :: AppState -> Application
app r = serveWithContext serverAPI (authContext r) (server r)

server :: AppState -> Server ServerAPI
server r = enter (appToUnderlying r) appServer
  where
    appServer = privateServer :<|> publicServer

privateServer :: Session -> ServerT PrivateAPI App
privateServer s = 
        rootServer
  :<|>  argumentsServer
  :<|>  claimsServer s
  :<|>  usersServer s

publicServer :: ServerT PublicAPI App
publicServer = login
