{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Arbital.Endpoints 
  ( app
  ) where

import Servant
import Servant.Server (serveWithContext)

import Arbital.Types
import Arbital.Endpoints.Arguments
import Arbital.Endpoints.Claims
import Arbital.Endpoints.Users
import Arbital.Endpoints.Authenticated

-- GET to / to get a page of all claims
type RootAPI = Get '[JSON] [ClaimItem]

rootServer :: Server RootAPI
rootServer = getAllClaimItems

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
app r = serveWithContext serverAPI authContext (server r)

server :: AppState -> Server ServerAPI
server r = enter (appToUnderlying r) appServer
  where
    appServer = privateServer :<|> publicServer

privateServer :: Session -> ServerT PrivateAPI App
privateServer _ = 
        rootServer
  :<|>  argumentsServer
  :<|>  claimsServer
  :<|>  usersServer

publicServer :: ServerT PublicAPI App
publicServer = login
