module Main

-- do action more
-- action >>= \_ => more
printTwoThings : IO ()
printTwoThings = do
  putStrLn "Hello"
  putStrLn "World"

printLength' : IO ()
printLength' = do
  putStr "Input String: "
  input <- getLine
  -- notice no 'in' required for do block
  let len = length input
  putStrLn $ show len

printLength : IO ()
printLength =
  putStr "Input string: " >>= \_ =>
  getLine >>= \input =>
  let len = length input in
  putStrLn $ show len

printLonger : IO ()
printLonger = do
  putStr "Input String 1: "
  input1 <- getLine
  let lenInput1 = Strings.length input1
  putStr "Input String 2: "
  input2 <- getLine
  let lenInput2 = Strings.length input2
  case compare lenInput1 lenInput2 of
    GT => putStrLn $ input1 ++ " " ++ (show lenInput1)
    EQ => putStrLn $ "Equal lengths: " ++ (show lenInput1)
    LT => putStrLn $ input2 ++ " " ++ (show lenInput2)

main : IO ()
main = do
  -- printLength
  -- printLength'
  -- printTwoThings
  printLonger
