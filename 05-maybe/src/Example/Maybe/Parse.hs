module Example.Maybe.Parse
  ( parseArgs
  ) where

import Data.Char (isDigit)

-- This function will return `Nothing` if there is an arg that
-- is not a number, otherwise will return `Just args`.
parseArgs :: [String] -> Maybe [Int]
parseArgs args = if length nonNumberArgs == 0
  then Just $ map read args
  else Nothing
  where
    nonNumberArgs = filter (not . all isDigit) args
