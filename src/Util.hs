module Util where

-- * Utility functions, common expressions

-- | Add arguments to the front and end of a list respectively
surround :: a -> a -> [a] -> [a]
surround front back = (front :) . (++ [back])
