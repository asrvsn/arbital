module Arbital.Database.Driver 
  ( Connection
  , newDbConnection
  ) where

data Connection

newDbConnection :: IO Connection
newDbConnection = undefined

