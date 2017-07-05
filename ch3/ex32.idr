import Data.Vect

myLength : List a -> Nat
myLength [] = 0
myLength (x :: xs) = 1 + myLength xs

myReverse : List a -> List a
myReverse = go Nil
  where
    go : List a -> List a -> List a
    go acc Nil = acc
    go acc (x :: xs) = go (x :: acc) xs

mapList : (a -> b) -> List a -> List b
mapList f [] = []
mapList f (x :: xs) = (f x) :: mapList f xs

mapVect : (a -> b) -> Vect n a -> Vect n b
mapVect f [] = []
mapVect f (x :: xs) = f x :: mapVect f xs
