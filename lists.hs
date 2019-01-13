fact :: Integer -> Integer
fact 0 = 1
fact 1 = 1
fact 2 = 2
fact n = n * fact (n - 1)

null' :: [a] -> Bool
null' [] = True
null' _ = False

head' :: [a] -> a
head' [] = error "The empty list has no head"
head' (x:xs) = x

tail' :: [a] -> [a]
tail' [] = error "Empty list"
tail' (x:xs) = xs

take' :: Integer -> [a] -> [a]
take' n [] = []
take' 0 _ = []
take' n (x:xs) = x : take' (n - 1) xs

drop' :: Integer -> [a] -> [a]
drop' n [] = []
drop' 0 xs = xs
drop' n (x:xs) = drop' (n - 1) xs

(!) :: [a] -> Integer -> a
[]     ! n = error "Invalid index"
(x:xs) ! 0 = x
(x:xs) ! n = xs ! (n - 1)

length' :: [a] -> Integer
length' [] = 0
length' (x:xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' = helper []
    where
        helper :: [a] -> [a] -> [a]
        helper acc [] = acc
        helper acc (x:xs) = helper (x:acc) xs

magic = 1 : 1 : zipWith (+) (tail magic) magic

map' ::  (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs
