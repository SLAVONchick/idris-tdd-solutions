data Tree : Type -> Type where
  Empty : Ord a => Tree a
  Node : Ord a => (left: Tree a) -> (val: a) -> (right: Tree a) -> Tree a

insert : a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
                                           LT => Node (insert x left) val right
                                           EQ => orig
                                           GT => Node left val (insert x right)

(++) : Tree a -> Tree a -> Tree a
(++) Empty y = y
(++) (Node left val right) y = left ++ right ++ insert val y

listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x Empty ++ listToTree xs

treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left val right) = (treeToList left) ++ val :: treeToList right


data Expr = Single Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr

eval : Expr -> Int
eval (Single x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mult x y) = eval x * eval y

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing right@(Just x) = right
maxMaybe left@(Just x) Nothing = left
maxMaybe left@(Just x) right@(Just y) = case compare x y of
                                  LT => right
                                  EQ => left
                                  GT => left


data Shape =
   Triangle Double Double
  | Rectangle Double Double
  | Circle Double


data Picture =
   Primitive Shape
  | Combine Picture Picture
  | Rotate Double Picture
  | Translate Double Double Picture

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive x) = case x of
                                  Triangle d1 d2 => Just (d1 + d2)
                                  _ => Nothing
biggestTriangle (Combine x y) =
                let fst = biggestTriangle x in
                let snd = biggestTriangle y in
                case fst of
                  Just tr => case snd of
                              Just tr2 => if tr > tr2 then fst else snd
                              Nothing => fst
                  Nothing => snd
biggestTriangle (Rotate _ y) = biggestTriangle y
biggestTriangle (Translate _ _ z) = biggestTriangle z


pic1 : Picture
pic1 = Combine (Primitive (Triangle 2 3)) (Primitive (Triangle 2 4))

pic2 : Picture
pic2 = Combine (Primitive (Rectangle 1 3)) (Primitive (Circle 4))
