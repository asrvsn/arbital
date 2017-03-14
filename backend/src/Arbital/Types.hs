{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Arbital.Types 
  (
  -- * Users
    Email(..)
  , UserID
  , User(..)
  -- * Sessions
  , SessionID(..)
  , Session(..)
  , setLastUsed
  -- * Claims
  , ClaimID(..)
  , Claim(..)
  , ClaimItem(..)
  -- * Arguments
  , ArgumentID(..)
  , Argument(..)
  , ArgumentItem(..)
  -- * Commits
  , CommitID(..)
  , Commit(..)
  , CommitAction(..)
  ) where

import GHC.Generics

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import Web.HttpApiData

-- * Users

newtype Email = Email Text deriving (Generic)

instance ToJSON Email
instance FromJSON Email

instance FromHttpApiData Email where
  parseUrlPiece = Right . Email

type UserID = Email

data User = User 
  { userId :: UserID
  , userEmail :: Email
  , userName :: Text
  , registrationDate :: UTCTime
  } 

instance ToJSON User where
  toJSON u = object [ "id" .= userId u
                    , "email" .= userEmail u
                    , "name" .= userName u
                    , "registrationDate" .= registrationDate u
                    ]
instance FromJSON User where
  parseJSON = withObject "user" $ \v -> 
    User <$> v .: "id"
         <*> v .: "email"
         <*> v .: "name"
         <*> v .: "registrationDate"

-- * Sessions

newtype SessionID = SessionID Text deriving (Generic, Ord, Eq)

instance ToJSON SessionID

data Session = Session 
  { sessionId :: SessionID
  , sessionUser :: User
  , sessionCreated :: UTCTime
  , sessionLastUsed :: UTCTime
  }

instance ToJSON Session where
  toJSON s = object [ "id" .= sessionId s
                    , "user" .= sessionUser s
                    , "created" .= sessionCreated s
                    ]

setLastUsed :: UTCTime -> Session -> Session
setLastUsed t s = s { sessionLastUsed = t }

-- * Claims

newtype ClaimID = ClaimID Text deriving (Generic)

instance ToJSON ClaimID
instance FromJSON ClaimID

instance FromHttpApiData ClaimID where
  parseUrlPiece = Right . ClaimID

data Claim = Claim
  { claimId :: ClaimID
  , claimText :: Text
  , argsFor :: [ArgumentID]
  , argsAgainst :: [ArgumentID]
  , claimAuthorId :: UserID
  , claimCreationDate :: UTCTime
  } 

instance ToJSON Claim where
  toJSON c = object [ "id" .= claimId c
                    , "text" .= claimText c
                    , "argsFor" .= argsFor c
                    , "argsAgainst" .= argsAgainst c
                    , "authorId" .= claimAuthorId c
                    , "creationDate" .= claimCreationDate c
                    ]
instance FromJSON Claim where
  parseJSON = withObject "claim" $ \v -> 
    Claim <$> v .: "id"
          <*> v .: "text"
          <*> v .: "argsFor"
          <*> v .: "argsAgainst"
          <*> v .: "authorId"
          <*> v .: "creationDate"

data ClaimItem = ClaimItem
  { iClaimId :: ClaimID
  , iClaimText :: Text
  , iClaimAuthorId :: UserID
  , iClaimAuthorName :: Text
  }

instance ToJSON ClaimItem where
  toJSON c = object [ "id" .= iClaimId c
                    , "text" .= iClaimText c
                    , "authorId" .= iClaimAuthorId c
                    , "authorName" .= iClaimAuthorName c
                    ]

-- * Arguments

newtype ArgumentID = ArgumentID Text deriving (Generic)

instance ToJSON ArgumentID
instance FromJSON ArgumentID

instance FromHttpApiData ArgumentID where
  parseUrlPiece = Right . ArgumentID

data Argument = Argument 
  { argumentId :: ArgumentID
  , argumentSummary :: Text
  , argumentClaims :: [ClaimID]
  , argumentAuthorId :: UserID
  , argumentCreationDate :: UTCTime
  } 

instance ToJSON Argument where
  toJSON a = object [ "id" .= argumentId a
                    , "text" .= argumentSummary a
                    , "claims" .= argumentClaims a
                    , "authorId" .= argumentAuthorId a
                    , "creationDate" .= argumentCreationDate a
                    ]
instance FromJSON Argument where
  parseJSON = withObject "argument" $ \v -> 
    Argument <$> v .: "id"
             <*> v .: "text" 
             <*> v .: "claims"
             <*> v .: "authorId"
             <*> v .: "creationDate"

data ArgumentItem = ArgumentItem 
  { iArgumentId :: ArgumentID
  , iArgumentSummary :: Text
  , iArgumentAuthorId :: UserID
  , iArgumentAuthorName :: Text
  } 

instance ToJSON ArgumentItem where
  toJSON a = object [ "id" .= iArgumentId a
                    , "text" .= iArgumentSummary a
                    , "authorId" .= iArgumentAuthorId a
                    , "authorName" .= iArgumentAuthorName a
                    ]

-- * Commits

newtype CommitID = CommitID Text

data Commit = Commit 
  { commitId :: CommitID
  , commitAuthor :: UserID
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
  deriving ( Generic )

instance ToJSON CommitAction
instance FromJSON CommitAction
