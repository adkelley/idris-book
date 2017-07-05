import Data.Vect

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) =
  let xsTrans = transposeMat xs
  in zipWith (::) x xsTrans

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

multHelper : Num a => (x : Vect m a) -> (ys : Vect p (Vect m a)) -> Vect p a
multHelper x [] = []
multHelper x (y :: ys) = sum (zipWith (*) x y) :: multHelper x ys

multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix xs ys = multMatrixT xs (transposeMat ys)
  where
    multMatrixT : Num a => Vect n (Vect m a) -> Vect p (Vect m a) -> Vect n (Vect p a)
    multMatrixT [] ys = []
    multMatrixT (x :: xs) ys = multHelper x ys :: multMatrixT xs ys
