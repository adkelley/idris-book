data BSTree : Type -> Type where
  Empty : Ord elem => BSTree elem
  Node  : Ord elem => (left : BSTree elem) -> (val : elem) ->
                      (right : BSTree elem) -> BSTree elem

-- Alternate way to define a BSTree data constructor
-- data BSTree elem = Empty
--                  | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) =
  case compare x val of
    LT => Node (insert x left) val right
    EQ => orig
    GT => Node left val (insert x right)
