{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Arbital.Endpoints.Claims
  ( ClaimsAPI
  , claimsServer
  , retrieveAllClaimItems
  ) where

import Servant

import Control.Monad ((>=>))

import Arbital.Types
import Arbital.State
import Arbital.Database.Driver
import Arbital.Database.Items

type ClaimsAPI = 
  -- POST to /claims to create a new claim
       "claims" :> "create" :> ReqBody '[JSON] Claim :> Post '[JSON] Claim
  
  :<|> "claims" :> Capture "claimid" ClaimID :> "for" :> ReqBody '[JSON] Argument :> Post '[JSON] Argument

  :<|> "claims" :> Capture "claimid" ClaimID :> "against" :> ReqBody '[JSON] Argument :> Post '[JSON] Argument

  -- GET to /claims/:claimid/items to read a claim's argument items
  :<|> "claims" :> Capture "claimid" ClaimID :> "items" :> "for" :> Get '[JSON] [ArgumentItem]

  :<|> "claims" :> Capture "claimid" ClaimID :> "items" :> "against" :> Get '[JSON] [ArgumentItem]

  -- GET to /claims/:claimid to read a claim
  :<|> "claims" :> Capture "claimid" ClaimID :> Get '[JSON] Claim

  :<|> "claims" :> "items" :> Capture "claimid" ClaimID :> Get '[JSON] ClaimItem

  :<|> "claims" :> "items" :> Get '[JSON] [ClaimItem]

claimsServer :: Session -> ServerT ClaimsAPI App
claimsServer s = 
        newClaim s
  :<|>  newArgFor s
  :<|>  newArgAgainst s
  :<|>  retrieveClaimArgsFor
  :<|>  retrieveClaimArgsAgainst
  :<|>  retrieveClaim
  :<|>  retrieveClaimItem
  :<|>  retrieveAllClaimItems

newClaim :: Session -> Claim -> App Claim
newClaim s c = withDb $ do
  c' <- createClaim c
  update Proxy (userId $ sessionUser s) (addClaim $ claimId c')
  return c'
  where
    addClaim i_c u = u { userClaims = i_c : userClaims u}

newArgFor :: Session -> ClaimID -> Argument -> App Argument
newArgFor s i a = withDb $ do
  a' <- createArgument a
  update Proxy i (addArgFor (argumentId a')) 
  update Proxy (userId $ sessionUser s) (addArg $ argumentId a')
  return a'
  where
    addArgFor i_a c = c { argsFor = i_a : argsFor c }
    addArg i_a u = u { userArguments = i_a : userArguments u }

newArgAgainst :: Session -> ClaimID -> Argument -> App Argument
newArgAgainst s i a = withDb $ do
  a' <- createArgument a
  update Proxy i (addArgAgainst (argumentId a')) 
  update Proxy (userId $ sessionUser s) (addArg $ argumentId a')
  return a'
  where
    addArgAgainst i_a c = c { argsAgainst = i_a : argsAgainst c }
    addArg i_a u = u { userArguments = i_a : userArguments u }

retrieveClaimArgsFor :: ClaimID -> App [ArgumentItem]
retrieveClaimArgsFor i = withDb $ 
  getClaim i >>= (mapM (getArgument >=> getArgumentItem) . argsFor)

retrieveClaimArgsAgainst :: ClaimID -> App [ArgumentItem]
retrieveClaimArgsAgainst i = withDb $ 
  getClaim i >>= (mapM (getArgument >=> getArgumentItem) . argsAgainst)

retrieveClaim :: ClaimID -> App Claim
retrieveClaim = withDb . getClaim

retrieveClaimItem :: ClaimID -> App ClaimItem
retrieveClaimItem = withDb . (getClaim >=> getClaimItem)

retrieveAllClaimItems :: App [ClaimItem]
retrieveAllClaimItems = withDb $ selectAll Proxy >>= mapM getClaimItem