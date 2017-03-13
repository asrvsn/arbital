module Arbital.Database.User 
  ( get
  , create
  ) where

import Data.Text (Text)
import Servant

import Arbital.Types

get :: UserID -> Handler (Maybe User)
get = undefined

create :: Email -> Text -> Handler User
create = undefined