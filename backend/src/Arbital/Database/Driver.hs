{-# LANGUAGE OverloadedStrings #-}

module Arbital.Database.Driver 
  ( Connection
  , openConnection
  , closeConnection
  ) where

import Hasql.Connection

openConnection :: IO (Either ConnectionError Connection)
openConnection = acquire dbSettings 

closeConnection :: Connection -> IO ()
closeConnection = release

dbSettings :: Settings
dbSettings = settings "localhost" 5432 "anand" "" "arbital"