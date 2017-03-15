module Main where

import System.Exit
import System.Posix.Signals
import Control.Concurrent (myThreadId)
import Control.Exception (throwTo)
import Control.Monad (void)
import Network.Wai.Handler.Warp
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
      let onCtrlC = closeConnection c >> throwTo tid ExitSuccess
      installHandler keyboardSignal (Catch onCtrlC) Nothing
      initApp c
      r <- newAppState c
      putStrLn $ "[Info] starting app"
      run 5000 (app r)

initApp :: Connection -> IO ()
initApp c = do
  void $ runDb c $ do
    createTable (Proxy :: Proxy User) 
    createTable (Proxy :: Proxy Claim)
    createTable (Proxy :: Proxy Argument)
    createTable (Proxy :: Proxy Commit)
