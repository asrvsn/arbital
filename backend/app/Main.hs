module Main where

import Network.Wai.Handler.Warp

import Arbital.Endpoints
import Arbital.State

main :: IO ()
main = do
  r <- newAppState
  run 5000 (app r)