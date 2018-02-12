module Main where

-- Import everything from the Lib.hs file.
import Lib

main :: IO ()
main = do
  putStrLn "addOne:"
  print $ addOne [10]
  print $ addOne [-1, 2]
  print $ addOne $ addOne [4, 5]

  putStrLn "\naddOne':"
  -- Uncomment this line out to see the error
  -- print $ addOne' []
  print $ addOne' [2]
  print $ addOne' [2,3]
  print $ addOne' [2,3,4]

  putStrLn "\npad10:"
  print $ pad10 []
  print $ pad10 [1]
  print $ pad10 [1,2]
  print $ pad10 [1,2,3,4,5,6,7,8,9]
  print $ pad10 [1, 2,3,4,5,6,7,8,9,9]

  putStrLn "\npad:"
  print $ pad 1 []
  print $ pad 2 []
  print $ pad 3 []
  print $ pad 3 [1,1]
  print $ pad 3 [1,1,1]

  putStrLn "\npad10':"
  print $ pad10' []
  print $ pad10' [1]
  print $ pad10' [1,2]
  print $ pad10' [1,2,3,4,5,6,7,8,9]
  print $ pad10' [1, 2,3,4,5,6,7,8,9,9]

  putStrLn "\npad10AndAdd:"
  print $ pad10AndAdd []
  print $ pad10AndAdd [1,2,3,4,5]
  print $ pad10AndAdd [1,2,3,4,5,6,7,8,9,0]

  putStrLn "\ngenericPad:"
  let lpad = genericPad True
  print $ lpad 0 10 []
  print $ lpad 1 5 []
  print $ lpad 1 5 [10,20,30,40,50,60]
  let rpad = genericPad False
  print $ rpad 17 5 []
  print $ rpad 17 3 [1]
  print $ rpad 17 3 [1,2]
  print $ rpad 17 3 [1,2,3]
