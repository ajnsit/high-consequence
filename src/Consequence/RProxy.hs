{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
module Consequence.RProxy (RProxy(RP)) where

import Data.Row

-- | Reified rows
data RProxy (r :: Row *) = RP

-- Sample Usage -------------------------------------
-- Meant to be used with TypeApplications.
-- Type signatures can be inferred, they are included here for clarity

_empty :: RProxy Empty
_empty = RP @Empty

_foo :: RProxy ("name" .== Int .+ "age" .== Int)
_foo = RP @("name" .== Int .+ "age" .== Int)
