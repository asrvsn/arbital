{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Arbital.State 
  ( 
  -- * Types
    AppT
  , App
  , AppState(..)
  -- * State
  , newAppState
  , runAppT
  , appToUnderlying
  -- * State manipulation
  , useSession
  , startSession
  , removeStaleSessions
  , withDb
  ) where

import           Data.Map (Map)
import           Data.Monoid
import           Data.Time.Clock 
import qualified Data.Map as Map
import           Servant 
import           Control.Concurrent.STM
import           Control.Monad.Reader

import Arbital.Database.Driver 
import Arbital.Types
import Arbital.Missing

  -- * Types

data AppState = AppState
  { dbConnection :: Connection
  , sessions :: TVar (Map SessionID Session)
  }

type AppT = ReaderT AppState  

type App = AppT Handler

  -- * State

newAppState :: Connection -> IO AppState
newAppState c = 
  AppState <$> pure c
           <*> newTVarIO Map.empty

runAppT :: AppState -> AppT m a -> m a
runAppT r m = runReaderT m r

appToUnderlying :: AppState -> AppT m :~> m
appToUnderlying r = Nat (runAppT r)

  -- * State manipulation
 
useSession :: SessionID -> App (Maybe Session)
useSession s = do
  t <- liftIO getCurrentTime
  ms <- asks sessions
  liftIO $ atomically $ do
    modifyTVar ms (Map.adjust (setLastUsed t) s)
    ms_ <- readTVar ms
    return $ Map.lookup s ms_ 

startSession :: User -> App Session
startSession u = do
  s <- liftIO $ SessionID <$> freshUid
  t <- liftIO getCurrentTime
  ms <- asks sessions
  let se = Session s u t t 
  liftIO $ atomically $ 
    modifyTVar ms (Map.insert s se)
  return se

removeStaleSessions :: NominalDiffTime -> App ()
removeStaleSessions dt = do
  t <- liftIO getCurrentTime
  ms <- asks sessions
  liftIO $ atomically $ 
    modifyTVar ms (Map.filter (isStale t))
  where
    isStale t se = (dt `addUTCTime` sessionLastUsed se) < t

withConnection :: (Connection -> IO a) -> App a
withConnection m = asks dbConnection >>= liftIO . m

withDb :: DbSession a -> App a
withDb m = do
  res <- withConnection $ \c -> runDb c m
  case res of 
    Left e -> throwError $ err500 { errReasonPhrase = "Database error: " <> show e }
    Right r -> return r