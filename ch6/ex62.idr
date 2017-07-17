import Data.Vect

Matrix : Nat -> Nat -> Type
Matrix n m = Vect n (Vect m Int)

testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]

-- exercise 2 - See Printf.idr

TupleVect : (numArgs : Nat) -> Type -> Type
TupleVect Z vecType = ()
TupleVect (S k) vecType =  (vecType, TupleVect k vecType)

test : TupleVect 4 Nat
test = (1, 2, 3, 4, ())
