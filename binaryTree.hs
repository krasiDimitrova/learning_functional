import Prelude hiding(Maybe(..))
data BinaryTree a
    = Empty
    | Node a (BinaryTree a) (BinaryTree a)
    deriving (Show)

data BinaryTreeInt
    = EmptyInt
    | NodeInt Int BinaryTreeInt BinaryTreeInt
    deriving (Show)

instance Eq BinaryTreeInt where
    EmptyInt == EmptyInt = True
    (NodeInt x left1 right1) == (NodeInt y left2 right2) =  (x == y)
                                                              && (left1 == left2) 
                                                              && (right1 == right2)

instance (Eq a) => Eq (BinaryTree a) where
    Empty == Empty = True
    (Node x left1 right1) == (Node y left2 right2) =  (x == y)
                                                        && (left1 == left2) 
                                                        && (right1 == right2)

data Maybe a
    = Nothing
    | Just a
    deriving (Show)

instance (Eq a) => Eq (Maybe a) where
    Nothing == Nothing = True
    Just x  == Just y  = x == y
    _       == _       = False

instance (Ord a) => Ord (Maybe a) where
    Just x  <= Just y  = x <= y
    Nothing <= _       = True
    Just x  <= Nothing = False

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Empty        = Empty
mapTree f (Node x l r) = Node (f x) (mapTree f l) (mapTree f r)

treeToList :: BinaryTree a -> [a]
treeToList Empty        = []
treeToList (Node x l r) = [x] ++ treeToList l ++ treeToList r

sumTree :: (Num a) => BinaryTree a -> a
sumTree Empty = 0
sumTree (Node x l r) = x + sumTree l +  sumTree r

prodTree :: (Num a) => BinaryTree a -> a
prodTree Empty = 1
prodTree (Node x l r) = x * prodTree l * prodTree r

f x = if x > 0 then (x + 2)
