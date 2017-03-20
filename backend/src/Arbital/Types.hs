{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Arbital.Types 
  (
  -- * Users
    Email(..)
  , UserID(..)
  , Name(..)
  , User(..)
  , UserPage(..)
  -- * Sessions
  , SessionID(..)
  , Session(..)
  , setLastUsed
  -- * Claims
  , ClaimID(..)
  , Claim(..)
  , ClaimCreator(..)
  , ClaimPage(..)
  -- * Arguments
  , ArgumentID(..)
  , Argument(..)
  , ArgumentCreator(..)
  , ArgumentPage(..)
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

newtype Email = Email Text deriving (Show, Generic)

instance ToJSON Email
instance FromJSON Email

newtype Name = Name Text deriving (Show, Generic)

instance ToJSON Name
instance FromJSON Name

newtype UserID = UserID Text deriving (Show, Generic)

instance FromHttpApiData UserID where
  parseUrlPiece = Right . UserID

instance ToJSON UserID
instance FromJSON UserID

data User = User 
  { userId :: UserID
  , userEmail :: Email
  , userName :: Name
  , userClaims :: [ClaimID]
  , userArguments :: [ArgumentID]
  , registrationDate :: UTCTime
  } deriving (Show)

instance ToJSON User where
  toJSON u = object [ "id" .= userId u
                    , "email" .= userEmail u
                    , "name" .= userName u
                    , "claims" .= userClaims u
                    , "arguments" .= userArguments u
                    , "registrationDate" .= registrationDate u
                    ]
instance FromJSON User where
  parseJSON = withObject "user" $ \v -> 
    User <$> v .: "id"
         <*> v .: "email"
         <*> v .: "name"
         <*> v .: "claims"
         <*> v .: "arguments"
         <*> v .: "registrationDate"

data UserPage = UserPage 
  { pageUser :: User
  , pageUserClaims :: [Claim]
  , pageUserArgs :: [Argument]
  } deriving (Show)

instance ToJSON UserPage where
  toJSON p = object [ "user" .= pageUser p
                    , "claims" .= pageUserClaims p
                    , "args" .= pageUserArgs p
                    ]

-- * Sessions

newtype SessionID = SessionID Text deriving (Show, Generic, Ord, Eq)

instance ToJSON SessionID

data Session = Session 
  { sessionId :: SessionID
  , sessionUser :: User
  , sessionCreated :: UTCTime
  , sessionLastUsed :: UTCTime
  } deriving (Show)

instance ToJSON Session where
  toJSON s = object [ "id" .= sessionId s
                    , "user" .= sessionUser s
                    , "created" .= sessionCreated s
                    ]

setLastUsed :: UTCTime -> Session -> Session
setLastUsed t s = s { sessionLastUsed = t }

-- * Claims

newtype ClaimID = ClaimID Text deriving (Show, Generic)

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
  , claimAuthorName :: Name
  , claimCreationDate :: UTCTime
  } deriving (Show)

instance ToJSON Claim where
  toJSON c = object [ "id" .= claimId c
                    , "text" .= claimText c
                    , "argsFor" .= argsFor c
                    , "argsAgainst" .= argsAgainst c
                    , "authorId" .= claimAuthorId c
                    , "authorName" .= claimAuthorName c
                    , "creationDate" .= claimCreationDate c
                    ]
instance FromJSON Claim where
  parseJSON = withObject "claim" $ \v -> 
    Claim <$> v .: "id"
          <*> v .: "text"
          <*> v .: "argsFor"
          <*> v .: "argsAgainst"
          <*> v .: "authorId"
          <*> v .: "authorName"
          <*> v .: "creationDate"

data ClaimCreator = ClaimCreator 
  { claimCText :: Text
  , claimCArgs :: [ArgumentID]
  } deriving (Show)

instance FromJSON ClaimCreator where
  parseJSON = withObject "claimcreator" $ \v -> 
    ClaimCreator <$> v .: "text"
                 <*> v .: "args"

data ClaimPage = ClaimPage 
  { pageClaim :: Claim
  , pageArgsFor ::[Argument]
  , pageArgsAgainst :: [Argument]
  } deriving (Show)

instance ToJSON ClaimPage where
  toJSON p = object [ "claim" .= pageClaim p
                    , "argsFor" .= pageArgsFor p
                    , "argsAgainst" .= pageArgsAgainst p
                    ]

-- * Arguments

newtype ArgumentID = ArgumentID Text deriving (Show, Generic)

instance ToJSON ArgumentID
instance FromJSON ArgumentID

instance FromHttpApiData ArgumentID where
  parseUrlPiece = Right . ArgumentID

data Argument = Argument 
  { argumentId :: ArgumentID
  , argumentSummary :: Text
  , argumentClaims :: [ClaimID]
  , argumentAuthorId :: UserID
  , argumentAuthorName :: Name
  , argumentCreationDate :: UTCTime
  } deriving (Show)

instance ToJSON Argument where
  toJSON a = object [ "id" .= argumentId a
                    , "text" .= argumentSummary a
                    , "claims" .= argumentClaims a
                    , "authorId" .= argumentAuthorId a
                    , "authorName" .= argumentAuthorName a
                    , "creationDate" .= argumentCreationDate a
                    ]
instance FromJSON Argument where
  parseJSON = withObject "argument" $ \v -> 
    Argument <$> v .: "id"
             <*> v .: "text" 
             <*> v .: "claims"
             <*> v .: "authorId"
             <*> v .: "authorName"
             <*> v .: "creationDate"

data ArgumentCreator = ArgumentCreator
  { argumentCSummary :: Text
  , argumentCClaims :: [ClaimID]
  } deriving (Show)

instance FromJSON ArgumentCreator where
  parseJSON = withObject "argumentcreator" $ \v -> 
    ArgumentCreator <$> v .: "text"
                    <*> v .: "claims"

data ArgumentPage = ArgumentPage 
  { pageArg :: Argument
  , pageClaims :: [Claim]
  } deriving (Show)

instance ToJSON ArgumentPage where
  toJSON p = object [ "arg" .= pageArg p
                    , "claims" .= pageClaims p
                    ]

-- * Commits

newtype CommitID = CommitID Text deriving (Show)

data Commit = Commit 
  { commitId :: CommitID
  , commitAuthor :: UserID
  , commitAction :: CommitAction
  , commitCreationDate :: UTCTime
  , commitMessage :: Text
  } deriving (Show)

data CommitAction = 
    CreateClaim Claim
  | UpdateClaim 
      { beforeClaim :: Claim
      , afterClaim :: Claim
      }
  | CreateArgumentFor ClaimID Argument
  | CreateArgumentAgainst ClaimID Argument
  | UpdateArgument ClaimID ArgumentID
  | CreateUser User
  | UpdateUser 
      { oldUser :: User
      , newUser :: User
      }
  deriving (Show, Generic )

instance ToJSON CommitAction
instance FromJSON CommitAction
