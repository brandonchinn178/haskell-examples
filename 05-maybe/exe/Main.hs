module Main where

import Example.Maybe (getMin, getMax)
import Example.Maybe.Parse (parseArgs)

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Just args' -> do
      putStr "Minimum: "
      print $ getMin args'
      putStr "Maximum: "
      print $ getMax args'
    -- Using `Maybe` allowed us to gracefully show an error message!
    Nothing -> putStrLn "Could not parse arguments. Only non-negative integers allowed."
