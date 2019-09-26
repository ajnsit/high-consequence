module Util where

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
