module Lib
  ( unsafeParse
  , safeParse
  , getMin
  ) where

import Data.Char (isDigit)

-- This function calls `read`, which attempts to read a String
-- as an Int in this case, but it will error if a String does
-- not look like an Int.
--
-- This function uses `map`, which has the type
-- `(a -> b) -> [a] -> [b]`. In other words, it takes a function
-- and a list, and applies that function to every element in the
-- list, replacing it with the result of the function. In this
-- specific example, `read` has type `String -> Int`, so `map`
-- converts a list of Strings into a list of Ints by calling `read`
-- on each one.
unsafeParse :: [String] -> [Int]
unsafeParse args = map read args -- Note: this can also be written
                                 -- as `unsafeParse = map read`
                                 -- (Hint: currying).

-- This function will throw out all non-numbers in the given list
-- before calling `read`, guaranteeing that `read` won't fail.
--
-- Note: will not work with negative numbers.
safeParse :: [String] -> [Int]
safeParse = unsafeParse . filter (all isDigit)

-- Get the minimum value in the given list
getMin :: [Int] -> Int
getMin [] = error "Cannot call getMin on an empty list"
getMin (x:[]) = x
getMin (x:xs) = helper x $ getMin xs
  where
    helper x y = if x < y then x else y
