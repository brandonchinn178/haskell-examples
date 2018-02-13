module Main where

-- `do` is syntactic sugar specifically for `IO` functions
-- to make it look imperative. For now, just copy it until
-- learning about Monads.
main :: IO ()
main = do
  putStrLn "Running Lists..."

  -- `putStrLn` only works on `String`s; `print` works for anything
  -- that has a `Show` instance ("inherits from Show" for you OOP
  -- learners)
  putStr "Printing [1,2,3]: "
  print [1,2,3]

  putStr "Concatenating [1,2,3] and [4,5]: "
  print ([1,2,3] ++ [4,5])

  putStr "Using let: "
  -- This is how you assign variables. This syntax is only allowed
  -- in a `do` block. Usually, you would have to do `let x = 1 in print x`.
  let l1 = [1,2,3]
  print l1

  putStr "Using $: "
  -- The `$` operator takes everything on the right as the last argument
  -- for the part on the left. `a $ b $ c $ d` is the same as `a (b (c d))`.
  --
  -- Note that this operator isn't a Haskell builtin. It's an actual function
  -- with the type `(a -> b) -> a -> b`.
  print $ [1,2,3]

  putStr "Using the list constructor: "
  -- [1,2,3] is actually syntactic sugar for `1:2:3:[]`. The `:` operator
  -- is a constructor that takes the form `<first elem>:<rest of list>`
  print $ 1:2:3:[]

  -- Some common list operations
  
  putStr "First element of list: "
  print $ head l1

  putStr "Second element of list: "
  print $ l1 !! 1

  putStr "Last element of list: "
  print $ last l1

  putStr "Tail of list: "
  print $ tail l1

  putStr "Init of list: "
  print $ init l1

  -- `let` allows "pattern matching", to bind different parts of a result
  -- to different variables
  let one:rest = l1
  putStrLn "Pattern matching:"
  print one
  print rest

  -- Let's try to combine some of our knowledge now

  let one = head l1
  let two = head $ tail l1
  putStr "Add first two elements: "
  print $ one + two
