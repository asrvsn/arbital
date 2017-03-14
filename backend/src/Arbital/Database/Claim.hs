module Arbital.Database.Claim
  ( get
  , create
  ) where

import Arbital.Types
import Arbital.Database.Driver

get :: Connection -> ClaimID -> IO (Maybe Claim)
get = undefined

create :: Connection -> Claim -> IO Claim
create = undefined

