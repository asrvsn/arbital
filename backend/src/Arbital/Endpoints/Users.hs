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
       "users" :> Capture "userid" UserID :> "page" :> Get '[JSON] UserPage

  :<|> "users" :> Capture "userid" UserID :> Get '[JSON] User

  -- GET to /users to get a page of all users
  :<|> "users" :> Get '[JSON] [User]

usersServer :: Session -> ServerT UsersAPI App
usersServer _ = 
        retrieveUserPage
  :<|>  retrieveUser
  :<|>  retrieveAllUsers

retrieveUserPage :: UserID -> App UserPage
retrieveUserPage i = withDb $ do
  u <- getUser i
  cs <- mapM getClaim (userClaims u)
  as <- mapM getArgument (userArguments u)
  return $ UserPage u cs as

retrieveUser :: UserID -> App User
retrieveUser = withDb . getUser 

retrieveAllUsers :: App [User]
retrieveAllUsers = withDb $ selectAll Proxy