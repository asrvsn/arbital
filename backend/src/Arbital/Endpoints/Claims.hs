{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Arbital.Endpoints.Claims
  ( ClaimsAPI
  , claimsServer
  , getAllClaimItems
  ) where

import Servant

import Arbital.Types

type ClaimsAPI = 
  -- POST to /claims to create a new claim
       "claims" :> "create" :> ReqBody '[JSON] Claim :> Post '[JSON] Claim
  
  :<|> "claims" :> Capture "claimid" ClaimID :> "for" :> ReqBody '[JSON] Argument :> Post '[JSON] Argument

  :<|> "claims" :> Capture "claimid" ClaimID :> "against" :> ReqBody '[JSON] Argument :> Post '[JSON] Argument

  -- GET to /claims/:claimid/items to read a claim's argument items
  :<|> "claims" :> Capture "claimid" ClaimID :> "items" :> Get '[JSON] [ArgumentItem]

  -- GET to /claims/:claimid to read a claim
  :<|> "claims" :> Capture "claimid" ClaimID :> Get '[JSON] Claim

  :<|> "claims" :> "items" :> Capture "claimid" ClaimID :> Get '[JSON] ClaimItem

  :<|> "claims" :> "items" :> Get '[JSON] [ClaimItem]

claimsServer :: Server ClaimsAPI
claimsServer = 
        createClaim
  :<|>  createArgumentFor
  :<|>  createArgumentAgainst
  :<|>  getClaimArguments
  :<|>  getClaim
  :<|>  getClaimItem
  :<|>  getAllClaimItems

createClaim :: Claim -> Handler Claim
createClaim = undefined

createArgumentFor :: ClaimID -> Argument -> Handler Argument
createArgumentFor = undefined

createArgumentAgainst :: ClaimID -> Argument -> Handler Argument
createArgumentAgainst = undefined 

getClaimArguments :: ClaimID -> Handler [ArgumentItem]
getClaimArguments = undefined

getClaim :: ClaimID -> Handler Claim
getClaim = undefined 

getClaimItem :: ClaimID -> Handler ClaimItem
getClaimItem = undefined

getAllClaimItems :: Handler [ClaimItem]
getAllClaimItems = undefined 