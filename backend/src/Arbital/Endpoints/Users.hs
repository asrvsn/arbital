{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Arbital.Endpoints.Users
  ( UsersAPI
  , usersServer
  ) where

import Servant

import Arbital.Types
import Arbital.State
import Arbital.Database.Driver
import Arbital.Database.Items

type UsersAPI = 
  
  -- GET to /users/:userid to get a user
       "users" :> Capture "userid" UserID :> "items" :> "arguments" :> Get '[JSON] [ArgumentItem]

  :<|> "users" :> Capture "userid" UserID :> "items" :> "claims" :> Get '[JSON] [ClaimItem]

  :<|> "users" :> Capture "userid" UserID :> Get '[JSON] User

  -- GET to /users to get a page of all users
  :<|> "users" :> Get '[JSON] [User]

usersServer :: Session -> ServerT UsersAPI App
usersServer _ = 
        retrieveUserArgItems 
  :<|>  retrieveUserClaimItems 
  :<|>  retrieveUser
  :<|>  retrieveAllUsers

retrieveUserArgItems :: UserID -> App [ArgumentItem]
retrieveUserArgItems i = withDb $ do
  u <- getUser i
  as <- mapM getArgument (userArguments u)
  mapM getArgumentItem as

retrieveUserClaimItems :: UserID -> App [ClaimItem]
retrieveUserClaimItems i = withDb $ do 
  u <- getUser i 
  cs <- mapM getClaim (userClaims u)
  mapM getClaimItem cs

retrieveUser :: UserID -> App User
retrieveUser = withDb . getUser 

retrieveAllUsers :: App [User]
retrieveAllUsers = withDb $ selectAll Proxy