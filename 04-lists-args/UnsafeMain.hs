module Main where

import Lib (unsafeParse, getMin)

import System.Environment (getArgs)

main :: IO ()
main = do
  -- The `<-` operator "unwraps" or "extracts" things from
  -- an IO type. `getArgs` has the type `IO [String]`, so
  -- `args` has the type `[String]`.
  args <- getArgs
  print $ getMin $ unsafeParse args
