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
       "users" :> Capture "userid" UserID :> "arguments" :> Get '[JSON] [Argument]

  :<|> "users" :> Capture "userid" UserID :> "claims" :> Get '[JSON] [Claim]

  :<|> "users" :> Capture "userid" UserID :> Get '[JSON] User

  -- GET to /users to get a page of all users
  :<|> "users" :> Get '[JSON] [User]

usersServer :: Session -> ServerT UsersAPI App
usersServer _ = 
        retrieveUserArgs 
  :<|>  retrieveUserClaims 
  :<|>  retrieveUser
  :<|>  retrieveAllUsers

retrieveUserArgs :: UserID -> App [Argument]
retrieveUserArgs i = 
  withDb $ getUser i >>= (mapM getArgument . userArguments)

retrieveUserClaims :: UserID -> App [Claim]
retrieveUserClaims i = 
  withDb $ getUser i >>= (mapM getClaim . userClaims)

retrieveUser :: UserID -> App User
retrieveUser = withDb . getUser 

retrieveAllUsers :: App [User]
retrieveAllUsers = withDb $ selectAll Proxy