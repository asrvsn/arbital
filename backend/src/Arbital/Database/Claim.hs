module Arbital.Database.Claim
  ( get
  , create
  ) where

import Arbital.Types

get :: ClaimID -> IO (Maybe Claim)
get = undefined

create :: Claim -> IO Claim
create = undefined

