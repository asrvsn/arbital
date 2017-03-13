{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Arbital.Endpoints.Users
  ( UsersAPI
  , usersServer
  ) where

import Servant

import Arbital.Types

type UsersAPI = 
  -- POST to /users to create a new user
       "users" :> "create" :> ReqBody '[JSON] User :> Post '[JSON] User

  -- GET to /users/:userid to get a user
  :<|> "users" :> Capture "userid" UserID :> "items" :> "arguments" :> Get '[JSON] [ArgumentItem]

  :<|> "users" :> Capture "userid" UserID :> "items" :> "claims" :> Get '[JSON] [ClaimItem]

  :<|> "users" :> Capture "userid" UserID :> Get '[JSON] User

  -- GET to /users to get a page of all users
  :<|> "users" :> Get '[JSON] [User]

usersServer :: Server UsersAPI
usersServer = 
        createUser
  :<|>  getUserArgumentItems
  :<|>  getUserClaimItems
  :<|>  getUser
  :<|>  getAllUsers

createUser :: User -> Handler User 
createUser = undefined

getUserArgumentItems :: UserID -> Handler [ArgumentItem]
getUserArgumentItems = undefined

getUserClaimItems :: UserID -> Handler [ClaimItem]
getUserClaimItems = undefined 

getUser :: UserID -> Handler User
getUser = undefined

getAllUsers :: Handler [User]
getAllUsers = undefined