import Data.Vect

data VectUnknown : Type -> Type where
  MkVect : (len : Nat) -> Vect len a -> VectUnknown a

readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do
  x <- getLine
  xs <- readVectLen k
  pure (x :: xs)

printVect : Show a => VectUnknown a -> IO ()
printVect (MkVect len xs) =
  putStrLn $ show xs ++ " (length " ++ show len ++ ")"

readVect : IO (VectUnknown String)
readVect = do
  x <- getLine
  if (x == "")
    then pure (MkVect _ [])
    else do MkVect _ xs <- readVect
            pure (MkVect _ (x :: xs))
