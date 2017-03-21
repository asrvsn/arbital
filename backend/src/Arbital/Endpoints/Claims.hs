{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Arbital.Endpoints.Claims
  ( ClaimsAPI
  , claimsServer
  , retrieveAllClaims
  ) where

import Servant
import Data.Text
import Data.List (sortBy)

import Arbital.Types
import Arbital.State
import Arbital.Database.Driver
import Arbital.Database.Items

type ClaimsAPI = 
  -- POST to /claims to create a new claim
       "claims" :> "create" :> ReqBody '[JSON] ClaimCreator :> Post '[JSON] Claim
  
  :<|> "claims" :> Capture "claimid" ClaimID :> "for" :> ReqBody '[JSON] ArgumentCreator :> Post '[JSON] Argument

  :<|> "claims" :> Capture "claimid" ClaimID :> "against" :> ReqBody '[JSON] ArgumentCreator :> Post '[JSON] Argument

  :<|> "claims" :> Capture "claimid" ClaimID :> "page" :> Get '[JSON] ClaimPage

  :<|> "claims" :> Capture "claimid" ClaimID :> Get '[JSON] Claim

  :<|> "claims" :> "search" :> Capture "query" Text :> Get '[JSON] [Claim]

  :<|> "claims" :> Get '[JSON] [Claim]

claimsServer :: Session -> ServerT ClaimsAPI App
claimsServer s = 
        newClaim s
  :<|>  newArgFor s
  :<|>  newArgAgainst s
  :<|>  retrieveClaimPage
  :<|>  retrieveClaim
  :<|>  searchClaims
  :<|>  retrieveAllClaims

newClaim :: Session -> ClaimCreator -> App Claim
newClaim s cc = withDb $ do
  c <- createClaim s cc
  update Proxy (userId $ sessionUser s) (addClaim $ claimId c)
  return c
  where
    addClaim i_c u = u { userClaims = i_c : userClaims u}

newArgFor :: Session -> ClaimID -> ArgumentCreator -> App Argument
newArgFor s i ac = withDb $ do
  a <- createArgument s ac
  update Proxy i (addArgFor (argumentId a)) 
  update Proxy (userId $ sessionUser s) (addArg $ argumentId a)
  return a
  where
    addArgFor i_a c = c { argsFor = i_a : argsFor c }
    addArg i_a u = u { userArguments = i_a : userArguments u }

newArgAgainst :: Session -> ClaimID -> ArgumentCreator -> App Argument
newArgAgainst s i ac = withDb $ do
  a <- createArgument s ac
  update Proxy i (addArgAgainst (argumentId a)) 
  update Proxy (userId $ sessionUser s) (addArg $ argumentId a)
  return a
  where
    addArgAgainst i_a c = c { argsAgainst = i_a : argsAgainst c }
    addArg i_a u = u { userArguments = i_a : userArguments u }

retrieveClaimPage :: ClaimID -> App ClaimPage
retrieveClaimPage i = withDb $ do
  c <- getClaim i
  af <- mapM getArgument (argsFor c)
  aa <- mapM getArgument (argsAgainst c)
  return $ ClaimPage c af aa

retrieveClaim :: ClaimID -> App Claim
retrieveClaim = withDb . getClaim

searchClaims :: Text -> App [Claim]
searchClaims q = withDb $ search Proxy 5 (Field "text" q)

retrieveAllClaims :: App [Claim]
retrieveAllClaims = do
  cs <- withDb $ selectAll Proxy 
  return $ sortBy cmp cs
  where
    cmp c c' = if claimCreationDate c > claimCreationDate c' then LT else GT