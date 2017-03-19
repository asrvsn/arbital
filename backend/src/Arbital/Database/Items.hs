{-# LANGUAGE OverloadedStrings #-}

module Arbital.Database.Items
  ( 
  -- * Users
    getUser
  , createUser
  -- * Claims
  , getClaim
  , createClaim
  -- * Arguments
  , getArgument
  , createArgument
  ) where

import Data.Time.Clock (getCurrentTime)
import Data.Proxy
import Control.Monad.IO.Class (liftIO)

import Arbital.Database.Driver
import Arbital.Types
import Arbital.Missing

-- * Users

getUser :: UserID -> DbSession User
getUser i = do
  mu <- select Proxy i 
  case mu of 
    Nothing -> dbResultErr "user record not found"
    Just u -> return u

createUser :: Email -> Name -> DbSession User
createUser email name = do
  t <- liftIO getCurrentTime 
  i <- liftIO $ UserID <$> freshUid
  let u = User { userId = i
               , userEmail = email
               , userName = name
               , userClaims = []
               , userArguments = []
               , registrationDate = t 
               }
  insert Proxy u
  return u

-- * Claims

getClaim :: ClaimID -> DbSession Claim
getClaim i = do
  mc <- select Proxy i
  case mc of 
    Nothing -> dbResultErr "claim record not found"
    Just c -> return c

createClaim :: Session -> ClaimCreator -> DbSession Claim
createClaim se cc = do
  t <- liftIO getCurrentTime
  i <- liftIO $ ClaimID <$> freshUid
  let c = Claim { claimId = i
                , claimText = claimCText cc
                , argsFor = claimCArgs cc
                , argsAgainst = []
                , claimAuthorId = userId user
                , claimAuthorName = userName user
                , claimCreationDate = t
                }
  insert Proxy c
  return c
  where
    user = sessionUser se

-- * Arguments

getArgument :: ArgumentID -> DbSession Argument
getArgument i = do
  ma <- select Proxy i
  case ma of 
    Nothing -> dbResultErr "argument record not found"
    Just a -> return a

createArgument :: Session -> ArgumentCreator -> DbSession Argument
createArgument se ac = do
  t <- liftIO getCurrentTime
  i <- liftIO $ ArgumentID <$> freshUid
  let a = Argument { argumentId = i
                   , argumentSummary = argumentCSummary ac
                   , argumentClaims = argumentCClaims ac
                   , argumentAuthorId = userId user
                   , argumentAuthorName = userName user
                   , argumentCreationDate = t
                   }
  insert Proxy a
  return a
  where
    user = sessionUser se