apply :: (a -> b) -> a -> b
apply f a = f a

applyTwice :: (a -> a) -> a -> a
applyTwice f x =  f (f x)

compose :: (a -> b) -> (b -> c) -> (a -> c)
compose f g = g . f

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []     = []
filter' p (x:xs) = if p x then x : filter' p xs else filter' p xs

partition' :: (a -> Bool) -> [a] -> ([a], [a]) 
partition' p xs = ((filter' p xs) , (filter' (not . p) xs))

quickSort :: [Integer] -> [Integer]
quickSort [] = []
quickSort (x:xs) = lesser ++ [x] ++ greater
     where (lesser, greater) = partition' (<x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b 
foldr' _ nv []     = nv
foldr' f nv (x:xs) = f x (foldr' f nv xs)

insert :: Integer -> [Integer] -> [Integer]
insert y []     = [y]
insert y (x:xs) = if y < x then y : (x : xs) else x : insert y xs

insertionSort :: [Integer] -> [Integer]
insertionSort xs = foldr' insert [] xs

foldl' :: (a -> b -> b) -> b -> [a] -> b
foldl' _ nv []     = nv
foldl' f nv (x:xs) = foldl' f (f x nv) xs

cartesian :: [a] -> [b] -> [(a, b)]
cartesian xs ys = [(x, y) | x <- xs, y <- ys]

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

nats' :: [Integer]
nats' = iterate' (\x -> (x + 1)) 0

divides :: Integer -> Integer -> Bool
divides 0 _ = error "Division by 0"
divides a b = mod b a == 0

divisors :: Integer -> [Integer]
divisors a = [y | y <- [1..a], divides y a]

isPrime :: Integer -> Bool
isPrime a = length [x | x <- [1..a], divides x a] == 2

eratosthenes :: [Integer] -> [Integer]
eratosthenes [] = []
eratosthenes (x:xs) = x : eratosthenes (filter (not . (divides x)) xs)

primes' :: [Integer]
primes' = eratosthenes [2..100]

partitionPos :: Integer -> [(Integer, Integer)]
partitionPos n = [(x, (n - x))| x <- [1..(n - 1)]]

positive2Tupels :: [(Integer, Integer)]
positive2Tupels = concatMap partitionPos nats'

fact :: Integer -> Integer
fact 0 = 1
fact 1 = 1
fact 2 = 2
fact n = n * fact (n - 1)

facts :: [Integer]
facts = map fact nats'

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs :: [Integer]
fibs = map fib nats'

scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' _ nv [] = []
scanl' f nv (x:xs) = nv : scanl' f (f nv x) xs

facts' :: [Integer]
facts' = scanl' (*) 1 [1..10]