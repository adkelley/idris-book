allLengths : List a -> Nat
allLengths [] = 0
allLengths (x :: xs) = 1 + allLengths xs

xor : Bool -> Bool -> Bool
xor False y = y
xor True  y = not y

mutual
  isEven : Nat -> Bool
  isEven Z = True
  isEven (S k) = isOdd k

  isOdd : Nat -> Bool
  isOdd Z = False
  isOdd (S k) = isEven k
