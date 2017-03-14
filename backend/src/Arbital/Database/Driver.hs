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

-- ** Users

instance Persistent Email where
  enc = contramap (\(Email e) -> e) (Enc.value Enc.text)
  dec = Email <$> Dec.value Dec.text

instance Persistent User where
  enc = 
        contramap userId enc
    <>  contramap userEmail enc
    <>  contramap userName (Enc.value Enc.text)
    <>  contramap registrationDate (Enc.value Enc.timestamptz)
  dec = User 
    <$> dec 
    <*> dec 
    <*> Dec.value Dec.text
    <*> Dec.value Dec.timestamptz

-- ** Sessions

