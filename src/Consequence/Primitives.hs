module Consequence.Primitives where

import Data.Text (Text)

-- These are the primitive column types we support
class Primitive a

-- POC: Currently we only support Integers, and Strings
instance Primitive Int
instance Primitive Text


-- TODO: Output tables must be composed of primitive only columns
