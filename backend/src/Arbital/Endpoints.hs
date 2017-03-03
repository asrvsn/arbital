{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Arbital.Endpoints where

import Servant.API

import Arbital.Types
import Arbital.Handlers

-- GET to / to get a page of all claims
type RootEndpoint = Get '[JSON] [ClaimItem]

type ArgumentsAPI = 
  "arguments" :> Capture "argumentid" ArgumentID :> 
    (    "items" :> Get '[JSON] [ClaimItem]

    :<|> Get '[JSON] Argument

    )

type ClaimsAPI = 
  -- POST to /claims to create a new claim
       "claims" :> "create" :> ReqBody '[JSON] Claim :> Post '[JSON] Claim
  
  -- POST to /claims/:claimid/.. to create arguments for/against it 
  :<|> "claims" :> Capture "claimid" ClaimID :> 
      (    "for" :> ReqBody '[JSON] Argument :> Post '[JSON] Argument

      :<|> "against" :> ReqBody '[JSON] Argument :> Post '[JSON] Argument

  -- GET to /claims/:claimid/items to read a claim's argument items
      :<|> "items" :> Get '[JSON] [ArgumentItem]

  -- GET to /claims/:claimid to read a claim
      :<|> Get '[JSON] Claim
      )

  :<|> "claims" :> "items" :> 
      (    Capture "claimid" ClaimID :>

      :<|> Get '[JSON] [ClaimItem]

      )

type UsersAPI = 
  -- POST to /users to create a new user
       "users" :> "create" :> ReqBody '[JSON] User :> Post '[JSON] User

  -- GET to /users/:userid to get a user
  :<|> "users" :> Capture "userid" UserID :> 
      (    "items" :> 
          (    "arguments" :> Get '[JSON] [ArgumentItem]

          :<|> "claims" :> Get '[JSON] [ClaimItem]

          )

      :<|> Get '[JSON] User

      )


  -- GET to /users to get a page of all users
  :<|> "users" :> Get '[JSON] [User]


type ServerAPI = RootEndpoint 
            :<|> ClaimsAPI
            :<|> UsersAPI

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

app :: Application
app = serve serverAPI server

server :: Server ServerAPI
server = root
    :<|>     createClaim
        :<|> getClaim
        :<|> getClaimsPage
        :<|>     postArgumentFor
            :<|> postArgumentAgainst
    :<|>     createUser
        :<|> getUserPage
        :<|> putUser
        :<|> getUsersPage
  where
    root    = getClaimsPage

