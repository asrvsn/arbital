{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Arbital.Types where

import GHC.Generics

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Time (UTCTime)

newtype UserID = UserID Text deriving (Generic)

instance ToJSON UserID
instance FromJSON UserID

data User = User 
  { userId :: UserID
  , email :: Text
  , name :: Text
  , registrationDate :: UTCTime
  }

newtype ClaimID = ClaimID Text deriving (Generic)

instance ToJSON ClaimID
instance FromJSON ClaimID

data Claim = Claim
  { claimText :: Text
  , argsFor :: [ArgumentID]
  , argsAgainst :: [ArgumentID]
  , claimOwner :: UserID
  , claimCreationDate :: UTCTime
  , claimId :: ClaimID
  } deriving (Generic)

instance ToJSON Claim
instance FromJSON Claim

newtype ArgumentID = ArgumentID Text deriving (Generic)

instance ToJSON ArgumentID
instance FromJSON ArgumentID

data Argument = Argument 
  { argumentSummary :: Text
  , argumentBody :: [ClaimID]
  , argumentOwner :: UserID
  , argumentCreationDate :: UTCTime
  , argumentID :: ArgumentID
  } deriving (Generic)

instance ToJSON Argument
instance FromJSON Argument

data Commit = Commit 
  { commitAuthor :: UserID
  , commitAction :: CommitAction
  , commitCreationDate :: UTCTime
  , commitMessage :: Text
  }

data CommitAction = 
    CreateClaim Claim
  | UpdateClaim 
      { oldClaim :: Claim
      , newClaim :: Claim
      }
  | CreateArgumentFor ClaimID Argument
  | CreateArgumentAgainst ClaimID Argument
  | UpdateArgument ClaimID ArgumentID
  | CreateUser User
  | UpdateUser 
      { oldUser :: User
      , newUser :: User
      }
