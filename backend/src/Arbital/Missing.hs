module Arbital.Missing
  ( freshUid 
  ) where

import           Data.UUID.V1 (nextUUID)
import           Data.UUID (toText)
import           Data.Text (Text)
import           Control.Concurrent (threadDelay)

freshUid :: IO Text
freshUid = do 
  mu <- nextUUID
  case mu of 
    Nothing -> threadDelay 20 >> freshUid
    Just u -> return (toText u)