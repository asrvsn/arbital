module Main where

import Network.Wai.Handler.Warp

import Arbital.Endpoints

main :: IO ()
main = run 5000 app