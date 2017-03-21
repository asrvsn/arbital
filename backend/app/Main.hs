{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Exit
import System.Posix.Signals
import Control.Concurrent (myThreadId)
import Control.Exception (throwTo)
import Control.Monad (void)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors 
import Network.Wai.Middleware.RequestLogger (logStdout)
import Data.Proxy

import Arbital.Types
import Arbital.State
import Arbital.Endpoints
import Arbital.Database.Driver

main :: IO ()
main = do
  ec <- openConnection
  case ec of 
    Left err -> do
      putStrLn $ "[Error] opening database connection failed: " ++ show err
      putStrLn "Exiting."
    Right c -> do
      tid <- myThreadId
      let onCtrlC = do closeConnection c 
                       putStrLn "[Info] Closed database connection."
                       throwTo tid ExitSuccess
      installHandler keyboardSignal (Catch onCtrlC) Nothing
      initApp c
      r <- newAppState c
      putStrLn $ "[Info] starting app"
      run 5000 (myCors . logStdout $ app r)

myCors :: Middleware
myCors = cors (const $ Just policy)
  where
    policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type", "servant-session-id"] }

initApp :: Connection -> IO ()
initApp c = void $ do
  -- runDb c $ dropTable (Proxy :: Proxy User) 
  -- runDb c $ dropTable (Proxy :: Proxy Claim)
  -- runDb c $ dropTable (Proxy :: Proxy Argument)
  -- runDb c $ dropTable (Proxy :: Proxy Commit)
  runDb c $ createTable (Proxy :: Proxy User) 
  runDb c $ createTable (Proxy :: Proxy Claim)
  runDb c $ createTable (Proxy :: Proxy Argument)
  runDb c $ createTable (Proxy :: Proxy Commit)
