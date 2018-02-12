-- This file will be run with `stack runghc`, which means it's the
-- entrypoint for the Haskell script.
-- As such, the module *must* be named `Main` and it *must* contain
-- a `main` function.
module Main where

-- `main` is of type `IO ()`. You can think of this type as: "`main`
-- is a function that interacts with the world and doesn't return
-- anything".
--
-- Note: the `()` type is called "Unit".
main :: IO ()
main = putStrLn "Hello world"
