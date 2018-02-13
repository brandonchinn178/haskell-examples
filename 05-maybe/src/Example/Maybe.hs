-- This module introduces the `Maybe` type. This type has two constructors:
-- `Nothing` and `Just`. The `Maybe` type is used to represent either the
-- existence of a result or the absence of.
--
-- Like with lists, you can pattern match and manipulate `Maybe` values that way:
--
--     invertUnless1 :: Maybe Int -> Maybe Int
--     invertUnless1 Nothing = Just 0
--     invertUnless1 (Just 1) = Just 1
--     invertUnless1 (Just _) = Nothing
--
-- If you want to pattern match within a function, use a `case` expression:
--
--     let x = Just 1
--     case x of
--       Just v -> putStrLn v -- will print 1
--       Nothing -> putStrLn "NOTHING"
module Example.Maybe
  ( getMin
  , getMax
  ) where

foldBy :: (Int -> Int -> Int) -> [Int] -> Maybe Int
-- The underscore is a wildcard; it matches anything. In this case,
-- we don't care what the function is for an empty list, so we use
-- a wildcard placeholder.
foldBy _ [] = Nothing
foldBy f (x:xs) = case foldBy f xs of
  Nothing -> Just x
  Just y -> Just $ f x y

-- Get the minimum value in the given list
getMin :: [Int] -> Maybe Int
getMin = foldBy $ \x y -> if x < y then x else y

-- Get the maximum value in the given list
getMax :: [Int] -> Maybe Int
getMax = foldBy $ \x y -> if x > y then x else y
