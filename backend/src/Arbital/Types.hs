module Arbital.Types where

import Data.Text (Text)
import Data.Time (UTCTime)

newtype UserID = UserID Text

data User = User 
  { userId :: UserID
  , email :: Text
  , name :: Text
  , registrationDate :: UTCTime
  }

newtype ClaimID = ClaimID Text

data Claim = Claim
  { claimText :: Text
  , argsFor :: [ArgumentID]
  , argsAgainst :: [ArgumentID]
  , claimOwner :: UserID
  , claimCreationDate :: UTCTime
  , claimId :: ClaimID
  }

instance ToJSON Claim
instance FromJSON Claim

newtype ArgumentID = ArgumentID Text

data Argument = Argument 
  { argumentBody :: [ClaimID]
  , argumentOwner :: UserID
  , argumentCreationDate :: UTCTime
  , argumentID :: ArgumentID
  }

instance ToJSON Argument
instance FromJSON Argument

data Commit = Commit 
  { commitAuthor :: UserID
  , commitAction :: CommitAction
  , commitCreationDate :: UTCTime
  }

instance ToJSON Commit
instance FromJSON Commit