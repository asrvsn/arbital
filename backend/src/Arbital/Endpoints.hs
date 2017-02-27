{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Arbital.Endpoints where

import Servant.API

import Arbital.Types
import Arbital.Handlers

-- homepage
type RootEndpoint = Get '[JSON] AllClaimsPage

type ClaimsAPI = 
  -- POST to /claims to create a new claim
       "claims" :> "create" :> ReqBody '[JSON] Claim :> Post '[JSON] Claim
  
  -- GET to /claims/:claimid to read a claim
  :<|> "claims" :> Capture "claimid" ClaimID :> Get '[JSON] ClaimPage

  -- GET to /claims to get a page of all claims
  :<|> "claims" :> Get '[JSON] AllClaimsPage

type UsersAPI = 
  -- POST to /users to create a new user
       "users" :> "create" :> ReqBody '[JSON] User :> Post '[JSON] User

  -- GET to /users/:userid to get a user's page
  :<|> "users" :> Capture "userid" UserID :> Get '[JSON] UserPage

  -- PUT to /users/:userid to update a user's info
  :<|> "users" :> Capture "userid" UserID :> ReqBody '[JSON] User :> Put '[JSON] User

  -- GET to /users to get a page of all users
  :<|> "users" :> Get '[JSON] AllUsersPage


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
    :<|>     createUser
        :<|> getUserPage
        :<|> putUser
        :<|> getUsersPage
  where
    root    = getClaimsPage

