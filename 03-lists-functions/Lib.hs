-- In this file, we will be defining List functions.
--
-- Now is a good time to talk about pure/impure functions. In the
-- context of Haskell programs, "pure" describes functions that
-- don't have side effects. What you provide as input completely
-- determines the output. On the other hand, "impure" describes
-- functions that have side effects. Things like input/output
-- (printing things to stdout, taking input from user), looking
-- at the file system, connecting to the Internet, etc.
--
-- It is good practice to do as much stuff in pure functions as
-- possible. If you have a script that takes input from the user
-- and makes everything capitalized, separate the pure function of
-- capitalizing a string, and use that function in an impure function
-- that reads input and writes output.
module Lib where

-- This function demonstrates adding `1` to the beginning of the
-- given list. The type here can be read as "take a list of Ints
-- and return another list of Ints". Since the return type is
-- not an `IO`, this is a pure function and does not have any
-- side effects.
addOne :: [Int] -> [Int]
addOne l = 1:l

-- Functions also pattern match on their arguments.
-- This function adds one after the first element in the list. Note
-- that apostrophes are valid identifier characters in Haskell. They
-- usually denote functions that do similar actions. This function
-- can be read "add one prime".
addOne' :: [Int] -> [Int]
-- Note: `error` is bad style. There are better ways of handling this
-- case, but for simplicity/learning, we'll use `error` here.
addOne' [] = error "Cannot run addOne' on an empty list"
-- x:xs is a common idiom for pattern matching on lists
addOne' (x:xs) = x:1:xs

-- Haskell doesn't, strictly speaking, have loops. Recursion is a
-- great way to think about how to solve a problem in Haskell. It's
-- a bit easier to reason about recursion in Haskell because you
-- can just match up the types.
pad10 :: [Int] -> [Int]
pad10 xs = if length xs < 10
  then pad10 $ 0:xs
  else xs

-- Here, we show a function with two arguments.
--
-- You might ask why we use arrows to separate both the arguments
-- and the return value. You can also think about this function
-- as taking an `Int` and returning *another function* of type
-- `[Int] -> [Int]`. In other words, this type signature is the
-- same as `Int -> ([Int] -> [Int])`.
--
-- This is called "currying", where functions take in a single input
-- and return another function. In Haskell, this is considered good
-- practice and is used pervasively. See the next function for a good
-- application of curried functions.
pad :: Int -> [Int] -> [Int]
pad len xs = if length xs < len
  then pad len $ 0:xs
  else xs

-- `pad10` can also be written in terms of pad. Here, we demonstate
-- the advantages of "currying". Since `pad` can be thought of as a
-- function that takes in a single input and returns another function,
-- we can partially apply it to one parameter and save the resulting
-- function to `pad10'`.
--
-- Remember that `pad` is of type `Int -> [Int] -> [Int]`.
-- So giving it just `10` will give `pad 10` the type `[Int] -> [Int]`
-- which is the desired type for `pad10'`.
pad10' :: [Int] -> [Int]
pad10' = pad 10

-- Function composition is also a powerful tool in Haskell. Just like
-- how math has the ∘ operator, where (f ∘ g)(x) is equivalent to
-- f(g(x)), Haskell has the . operator that evaluates the right
-- function and provides that as the argument for the left function.
pad10AndAdd :: [Int] -> [Int]
pad10AndAdd = addOne . pad10

-- `where` clauses expose helper functions that are only accessible
-- by the primary function. Functions in the `where` clause have access
-- to arguments of the function.
--
-- It's similar to a `let ... in ...` expression. Use whichever is
-- more convenient/easier to read. For a more in-depth comparison,
-- see https://wiki.haskell.org/Let_vs._Where.
genericPad :: Bool -> Int -> Int -> [Int] -> [Int]
genericPad isLeft fill len = doPad -- Note that `xs` is left out in the argument list. Why does this work?
  where
    -- This function uses "guards", which allow arbitrary conditionals
    -- in addition to pattern matching. Here, we have one branch for if
    -- xs needs padding and another for when it's done.
    doPad xs
      | length xs < len = doPad $ pad' xs
      | otherwise = xs
    pad' xs = if isLeft
      then fill:xs
      else xs ++ [fill]
