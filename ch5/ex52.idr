module Main

import System

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit $ unpack input
    then pure (Just $ cast input)
    else pure Nothing

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do
  putStrLn "Make a guess"
  Just input <- readNumber
      | Nothing => do putStrLn "Invalid input"
                      guess target guesses
  let anotherGuess =
    do putStrLn $ "Guesses thus far: " ++ (show $ succ guesses)
       putStrLn "Another guess?"
       yn <- getLine
       if yn == "y"
        then guess target (succ guesses)
        else do putStrLn $ "Number was: " ++ (show target)
                pure ()
  case (compare input target) of
    LT => do putStrLn "Too low!"
             anotherGuess
    EQ => do putStrLn "Congratulations!"
             pure ()
    GT => do putStrLn "Too high!"
             anotherGuess

main : IO ()
main = do
  t <- time
  let r = cast $ t `mod` 100
  guess r Z
