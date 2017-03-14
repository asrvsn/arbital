{-# LANGUAGE OverloadedStrings #-}

module Arbital.Database.Driver 
  ( 
  -- * Config
    Connection
  , openConnection
  , closeConnection
  -- * Persistent typeclass
  ) where

import           Data.Monoid
import           Data.Functor.Contravariant
import           Data.Time.Clock (UTCTime)
import           Data.Text (Text)
import           Data.Foldable (foldl')
import qualified Data.Aeson as A
import           Control.Monad (replicateM)
import           Hasql.Connection
import qualified Hasql.Encoders as Enc
import qualified Hasql.Decoders as Dec

import           Arbital.Types

-- * Config

openConnection :: IO (Either ConnectionError Connection)
openConnection = acquire dbSettings 

closeConnection :: Connection -> IO ()
closeConnection = release

dbSettings :: Settings
dbSettings = settings "localhost" 5432 "anand" "" "arbital"

-- * Persistent typeclass

class Persistent a where
  enc :: Enc.Params a
  dec :: Dec.Row a

class ValuePersistent a where
  encVal :: Enc.Value a
  decVal :: Dec.Value a

-- ** General

instance ValuePersistent UTCTime where
  encVal = Enc.timestamptz
  decVal = Dec.timestamptz

instance Persistent UTCTime where
  enc = Enc.value encVal
  dec = Dec.value decVal

instance ValuePersistent Text where
  encVal = Enc.text
  decVal = Dec.text

instance Persistent Text where
  enc = Enc.value encVal
  dec = Dec.value decVal

instance (ValuePersistent a) => ValuePersistent [a] where
  encVal = Enc.array (Enc.arrayDimension foldl' (Enc.arrayValue encVal))
  decVal = Dec.array (Dec.arrayDimension replicateM (Dec.arrayValue decVal))

-- ** Users

instance Persistent Email where
  enc = contramap (\(Email e) -> e) enc
  dec = Email <$> dec

instance Persistent User where
  enc = 
        contramap userId enc
    <>  contramap userEmail enc
    <>  contramap userName enc
    <>  contramap registrationDate enc
  dec = User 
    <$> dec 
    <*> dec 
    <*> dec
    <*> dec

-- ** Sessions

instance Persistent SessionID where
  enc = contramap (\(SessionID s) -> s) enc
  dec = SessionID <$> dec

instance Persistent Session where
  enc =
        contramap sessionId enc
    <>  contramap sessionUser enc
    <>  contramap sessionCreated enc
    <>  contramap sessionLastUsed enc
  dec = Session
    <$> dec
    <*> dec 
    <*> dec
    <*> dec

-- ** Claims

instance Persistent ClaimID where
  enc = contramap (\(ClaimID c) -> c) enc
  dec = ClaimID <$> dec

instance ValuePersistent ClaimID where
  encVal = contramap (\(ClaimID c) -> c) encVal
  decVal = ClaimID <$> decVal

instance Persistent Claim where
  enc = 
        contramap claimText enc
    <>  contramap argsFor (Enc.value encVal)
    <>  contramap argsAgainst (Enc.value encVal)
    <>  contramap claimAuthorId enc
    <>  contramap claimCreationDate enc
    <>  contramap claimId enc
  dec = Claim
    <$> dec
    <*> Dec.value decVal
    <*> Dec.value decVal
    <*> dec
    <*> dec
    <*> dec

-- ** Arguments

instance Persistent ArgumentID where
  enc = contramap (\(ArgumentID a) -> a) enc
  dec = ArgumentID <$> dec

instance ValuePersistent ArgumentID where
  encVal = contramap (\(ArgumentID a) -> a) encVal
  decVal = ArgumentID <$> decVal

instance Persistent Argument where
  enc = 
        contramap argumentSummary enc
    <>  contramap argumentClaims (Enc.value encVal)
    <>  contramap argumentAuthorId enc
    <>  contramap argumentCreationDate enc
    <>  contramap argumentId enc
  dec = Argument 
    <$> dec
    <*> Dec.value decVal
    <*> dec
    <*> dec
    <*> dec

-- ** Commits

instance Persistent CommitID where
  enc = contramap (\(CommitID c) -> c) enc
  dec = CommitID <$> dec

instance Persistent Commit where
  enc = 
        contramap commitAuthor enc
    <>  contramap commitAction enc
    <>  contramap commitCreationDate enc
    <>  contramap commitMessage enc
    <>  contramap commitId enc
  dec = Commit
    <$> dec
    <*> dec
    <*> dec
    <*> dec
    <*> dec

-- #partial
instance Persistent CommitAction where
  enc = contramap A.toJSON (Enc.value Enc.json)
  dec = from <$> Dec.value Dec.json
    where
      from a = case A.fromJSON a of 
        A.Error e -> error $ "Persistent/CommitAction: " ++ e 
        A.Success s -> s