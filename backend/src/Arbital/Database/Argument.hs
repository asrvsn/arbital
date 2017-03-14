module Arbital.Database.Argument
  ( get
  , create
  ) where

import Arbital.Types
import Arbital.Database.Driver
    
get :: Connection -> ArgumentID -> IO (Maybe Argument)
get = undefined

create :: Connection -> Argument -> IO Argument
create = undefined