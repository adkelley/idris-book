import Data.Vect

append : Vect n elem -> Vect m elem -> Vect (n + m) elem
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

zip : Vect n a -> Vect n b -> Vect n (a, b)
zip [] ys = []
zip (x :: xs) (y :: ys) = (x, y) :: Main.zip xs ys

||| Note we bring n from Vect n a into scope with {n}
tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} i xs =
  case integerToFin i n of
    Nothing => Nothing
    Just idx => Just (index idx xs)

vectTake : (n: Nat) -> Vect (n + m) a -> Vect n a
vectTake Z xs = []
vectTake (S k) (x :: xs) = x :: vectTake k xs


sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys =
  case integerToFin pos n of
    Nothing => Nothing
    Just idx => Just $ (+) (index idx xs) (index idx ys)

sumEntries' : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries' {n} pos xs ys =
  map (\idx => index idx xs + index idx ys) (integerToFin pos n)
