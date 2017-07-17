import Data.Vect

readToBlank : IO (List String)
readToBlank = go Nil where
  go : (acc : List String) -> IO (List String)
  go acc = do
    xs <- getLine
    if xs == ""
      then pure acc
      else go (xs :: acc)


readAndSave : IO ()
readAndSave = do
  putStrLn "Enter strings, followed by a blank line"
  listOfText <- readToBlank
  putStrLn "Enter a filename: "
  fname <- getLine
  writeFile fname (show listOfText) >>=
    either (\err => putStrLn $ show err) (\_ => putStrLn "success")


readVect : (h : File) -> IO (n ** Vect n String)
readVect h =
  fGetLine h >>=
    either
      (\_ => do closeFile h
                pure (0 ** []))
      (\x => if (x == "")
               then do closeFile h
                       pure (_ ** [])
               else do (_ ** xs) <- readVect h
                       pure (_ ** x :: xs))



readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename =
  openFile filename Read >>=
    either (\_ =>  pure (0 ** [])) (\h => readVect h)
