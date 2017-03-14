{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Arbital.Endpoints.Claims
  ( ClaimsAPI
  , claimsServer
  , getAllClaimItems
  ) where

import Servant

import Arbital.Types
import Arbital.State

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

claimsServer :: ServerT ClaimsAPI App
claimsServer = 
        createClaim
  :<|>  createArgumentFor
  :<|>  createArgumentAgainst
  :<|>  getClaimArguments
  :<|>  getClaim
  :<|>  getClaimItem
  :<|>  getAllClaimItems

createClaim :: Claim -> App Claim
createClaim = undefined

createArgumentFor :: ClaimID -> Argument -> App Argument
createArgumentFor = undefined

createArgumentAgainst :: ClaimID -> Argument -> App Argument
createArgumentAgainst = undefined 

getClaimArguments :: ClaimID -> App [ArgumentItem]
getClaimArguments = undefined

getClaim :: ClaimID -> App Claim
getClaim = undefined 

getClaimItem :: ClaimID -> App ClaimItem
getClaimItem = undefined

getAllClaimItems :: App [ClaimItem]
getAllClaimItems = undefined 