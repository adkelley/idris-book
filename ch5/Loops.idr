module Main

import System

-- :exec readNumber >>= printLn
readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit $ unpack input
    then pure (Just (cast input))
    else pure Nothing

countDown : (secs : Nat) -> IO ()
countDown Z = putStrLn "Lift Off!"
countDown (S secs) = do
  putStrLn (show (S secs))
  usleep 1000000
  countDown secs

countDowns : IO ()
countDowns = do
  putStrLn "Enter starting number: "
  Just startNum <- readNumber
      | Nothing => do putStrLn "Invalid input"
                      countDowns
  countDown startNum
  putStrLn "Another (y/n)? "
  yn <- getLine
  if yn == "y"
    then countDowns
    else pure ()
