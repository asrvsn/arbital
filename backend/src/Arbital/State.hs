module Arbital.State 
  ( 
  -- * Types
    AppT
  , App
  , AppState(..)
  -- * State
  , newAppState
  , appToUnderlying
  -- * State manipulation
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Servant 

import Arbital.Database.Driver 
import Arbital.Types

  -- * Types

data AppState = AppState
  { dbConnection :: Connection
  , sessions :: TVar (Map SessionID UserID)
  }

type AppT = ReaderT AppState  

type App = AppT Handler

  -- * State

newAppState :: IO AppState
newAppState = 
  AppState <$> newDbConnection
           <*> newTVarIO Map.empty

appToUnderlying :: AppState -> AppT m :~> m
appToUnderlying r = NT $ \m -> runReaderT m r

  -- * State manipulation
