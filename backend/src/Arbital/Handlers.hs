module Arbital.Handlers where

import Servant.API

import Arbital.Types

-- Claims

createClaim :: Claim -> Handler Claim
createClaim = undefined

getClaim :: ClaimID -> Handler Claim
getClaim = undefined

getClaimsPage :: Handler ClaimsPage
getClaimsPage = undefined

-- Users

createUser :: User -> Handler User
createUser = undefined

getUserPage :: UserID -> Handler UserPage
getUserPage = undefined

putUser :: UserID -> User -> Handler User
putUser = undefined

getUsersPage :: Handler UsersPage
getUsersPage = undefined
