data BSTree : Type -> Type where
  Empty : Ord elem => BSTree elem
  Node  : Ord elem => (left : BSTree elem) -> (val : elem) ->
                      (right : BSTree elem) -> BSTree elem

%name BSTree tree, tree1

insert : Ord elem => elem -> BSTree elem -> BSTree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) =
  case compare x val of
    LT => Node (insert x left) val right
    EQ => orig
    GT => Node left val (insert x right)


listToTree : Ord a => List a -> BSTree a
listToTree = foldr insert Empty


treeToList : Ord a => BSTree a -> List a
treeToList Empty = []
treeToList (Node left val right) = treeToList left ++ (val :: treeToList right)


data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr

%name Expr expr, expr1

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add expr expr1) = evaluate expr + evaluate expr1
evaluate (Sub expr expr1) = evaluate expr - evaluate expr1
evaluate (Mul expr expr1) = evaluate expr * evaluate expr1

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing (Just y) = Just y
maxMaybe (Just x) Nothing = Just x
maxMaybe (Just x) (Just y) =
  case compare x y of
    LT => Just y
    EQ => Just x
    GT => Just x

||| Ex 6.  See Biggest Triangle in Picture.idr    
