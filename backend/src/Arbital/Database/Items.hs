{-# LANGUAGE OverloadedStrings #-}

module Arbital.Database.Items
  ( 
  -- * Users
    getUser
  , createUser
  -- * Claims
  , getClaim
  , getClaimItem
  , createClaim
  -- * Arguments
  , getArgument
  , getArgumentItem
  , createArgument
  ) where

import Data.Time.Clock (getCurrentTime)
import Data.Proxy
import Data.Text (Text)
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

createUser :: Email -> Text -> DbSession User
createUser email name = do
  t <- liftIO getCurrentTime 
  i <- liftIO $ Email <$> freshUid
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

getClaimItem :: Claim -> DbSession ClaimItem
getClaimItem c = do
  u <- getUser (claimAuthorId c)
  return $ toClaimItem (userName u) c

createClaim :: Claim -> DbSession Claim
createClaim c = do
  t <- liftIO getCurrentTime
  i <- liftIO $ ClaimID <$> freshUid
  let c' = c { claimId = i, claimCreationDate = t }
  insert Proxy c'
  return c'

-- * Arguments

getArgument :: ArgumentID -> DbSession Argument
getArgument i = do
  ma <- select Proxy i
  case ma of 
    Nothing -> dbResultErr "argument record not found"
    Just a -> return a

getArgumentItem :: Argument -> DbSession ArgumentItem
getArgumentItem a = do
  u <- getUser (argumentAuthorId a)
  return $ toArgumentItem (userName u) a

createArgument :: Argument -> DbSession Argument
createArgument a = do
  t <- liftIO getCurrentTime
  i <- liftIO $ ArgumentID <$> freshUid
  let a' = a { argumentId = i, argumentCreationDate = t }
  insert Proxy a'
  return a'