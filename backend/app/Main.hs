module Main where

import System.Exit
import System.Posix.Signals
import Control.Concurrent (myThreadId)
import Control.Exception (throwTo)
import Network.Wai.Handler.Warp

import Arbital.Endpoints
import Arbital.State
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
      r <- newAppState c
      putStrLn $ "[Info] starting app"
      run 5000 (app r)
