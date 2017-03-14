module Arbital.Database.User 
  ( get
  , create
  ) where

import Data.Text (Text)

import Arbital.Types
import Arbital.Database.Driver

get :: Connection -> UserID -> IO (Maybe User)
get = undefined

create :: Connection -> Email -> Text -> IO User
create = undefined