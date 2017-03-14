{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Arbital.Endpoints.Users
  ( UsersAPI
  , usersServer
  ) where

import Servant

import Arbital.Types
import Arbital.State

type UsersAPI = 
  -- POST to /users to create a new user
       "users" :> "create" :> ReqBody '[JSON] User :> Post '[JSON] User

  -- GET to /users/:userid to get a user
  :<|> "users" :> Capture "userid" UserID :> "items" :> "arguments" :> Get '[JSON] [ArgumentItem]

  :<|> "users" :> Capture "userid" UserID :> "items" :> "claims" :> Get '[JSON] [ClaimItem]

  :<|> "users" :> Capture "userid" UserID :> Get '[JSON] User

  -- GET to /users to get a page of all users
  :<|> "users" :> Get '[JSON] [User]

usersServer :: ServerT UsersAPI App
usersServer = 
        createUser
  :<|>  getUserArgumentItems
  :<|>  getUserClaimItems
  :<|>  getUser
  :<|>  getAllUsers

createUser :: User -> App User 
createUser = undefined

getUserArgumentItems :: UserID -> App [ArgumentItem]
getUserArgumentItems = undefined

getUserClaimItems :: UserID -> App [ClaimItem]
getUserClaimItems = undefined 

getUser :: UserID -> App User
getUser = undefined

getAllUsers :: App [User]
getAllUsers = undefined