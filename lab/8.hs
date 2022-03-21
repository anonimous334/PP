import Data.List
import Data.Maybe
--Evaluarea lenesa (Lazy evaluation) = un tip de evaluarea normala

{-
Pentru "y = f(x)":

Evaluarea aplicativa:
    Evalueaza x (toti parametrii lui f)
    Evalueaza f
    Evalueaza f(x)

Evaluarea normala:
    Evalueaza f
    Evalueaza din x doar ce este necesar pentru f
    Evalueaza f(x)
-}

--Ce putem face cu evaluarea lenesa?
--Se considera functia infiniteList
infiniteList :: [Integer]
infiniteList = 1 : infiniteList

--Intr-un limbaj cu evaluare aplicativa, aceasta functie nu ar ajunge sa intoarca rezultatul din cauza auto-reapelarii fara conditie de oprire

--In Haskell aceasta functie va intoarce o lista infinita, care poate fi si printata

--Cum putem interactiona cu ea?
getOnesLazy :: Int -> [Integer]
getOnesLazy n = take n infiniteList

--Este exact ca:
getOnesBasic :: Int -> [Integer]
getOnesBasic n = op n
    where
        op 0 = []
        op n = 1 : op (n-1)

--EXERCITIUL 8.1.1 - lista infinita de numere naturale
nat :: [Integer]
nat = op 0
    where
        op x = x : op (x + 1)

--Functii ca take si head ne ajuta sa transformam lista infinita in date finite
zero :: Integer
zero = head nat

--Functii ca drop, tail, map, zipWith si foldr/foldl ne ajuta sa prelucram liste infinite, dar rezultatele vor ramane infinite
--Aceasta functie va intoarce numerele naturale pozitive
natPositive :: [Integer]
natPositive = tail nat

--EXERCITIUL 8.1.2 - lista infinita de numere naturale impare
oddStream :: [Integer]
oddStream = filter (odd) nat

oddStream1 :: [Integer]
oddStream1 = map (\x -> x * 2 + 1) nat

oddStream2 :: [Integer]
oddStream2 = zipWith (+) nat $ zipWith (+) nat ones
    where
        --ones = infiniteList 
        --ones = map (\x -> 1) nat
        ones = tail $ zipWith div nat nat

--EXERCITIUL 8.1.3 - lista infinita de numere Fibonacci
fibo :: [Integer]
fibo = op 0 1
    where
        op x y = x : op y (x + y)

--PENTRU EXERCITIILE 8.2.*:
--Folositi "fromInteger x" pentru a-l transforma pe x in Double
--daca este necesara o conversie explicita scrieti "fromInteger x :: Double"

gen = (* 3)

build :: (Integer -> Integer) -> Integer -> [Integer]
build g a0 = a0 : (build g (g a0)) 


getNextElem :: Integer -> [Integer] -> Integer
getNextElem x l = head $ drop (fromJust (elemIndex x l) + 1) l

getPrevElem :: Integer -> [Integer] -> Integer
getPrevElem x l = head $ drop (fromJust (elemIndex x l) - 1) l

select :: Integer -> [Integer] -> [Integer]
select e l = filter (\x -> abs(x - getNextElem x l) < e) lista
           where lista = reverse $ tail $ reverse l

ffibo :: [Double]
ffibo = map (\x -> fromInteger x :: Double) fibo

-- phi :: [Float]
-- phi = map (\x ->  x \ (getPrevElem x fibo)) (tail fibo)


listOfIn :: [Int] -> [Int]
listOfIn l = map (\x -> fromJust (elemIndex x l) ) l


allPrevs l = map (\x -> fromIntegral(x) / fromIntegral(getPrevElem x (tail l))) (tail $ tail l)

phi x = (x + sin x) : phi (x + sin x)

square x k = 0.5 * (x + k/x) : square (0.5 * (x + k/x)) k

-- seqq :: Integer -> [Integer] 
seqq 0 = []
seqq h = h : seqq (h * 0.5)

appr a h = 




{-
import Data.List

sieve :: [Integer] -> [Integer]
sieve (h : t) = h : sieve (filter (\y -> y `mod` h /= 0) t)

primes = sieve [2..]

{-
    8.1
-}

-- 1 --
nat :: [Integer]
nat = [1..]

-- 2 --
oddNat :: [Integer]
oddNat = filter odd nat

-- 3 --
fibo = [1, 1] ++ f 1 1
    where
        f a b = a + b : f b (a + b)

{-
    8.2    
-}

myCast x = fromInteger x :: Double

-- 1 --
build :: (t -> t) -> t -> [t]
build g a = a : build g (g a)

-- 2 --
select :: (Ord a, Num a) => a -> [a] -> a
select e (h1:h2:t)
    | abs (h1 - h2) < e = h1
    | otherwise = select e (h2:t)

-- 3 --
limFibo = zipWith (flip (/)) fibo $ tail fibo
fi = select 0.0001 limFibo

-- 4 --
piAprox = build (\a -> a + sin a) 4.0

myPi = select 0.0001 piAprox

-- 5 --
sqrAprox k = build (\x -> (1/2)*(x + k/x)) (k/2)

mySqr = select 0.0001 . sqrAprox

-- 6 --
newtonIteration f f' = build (\x -> if f' x == 0 then error "Can't divide by 0!! Please choose a better aproximation." else x - (f x / f' x))

root f f' = select 0.0001 . newtonIteration f f'


-- 7 --
seqH = build (/2) 1

aproxf' f x = map (\h -> (f (x + h) - f x) / h) seqH

derivative f x = select 0.0001 $ aproxf' f x 


-- 8 --
-- a.
area :: Fractional a => (a -> a) -> a -> a -> a
area f a b = (b - a) * (f b + f a) / 2

-- b.
expand :: Fractional a => [a] -> [a]
expand [] = []
expand [a] = [a]
expand (a:b:l) = a : (a + b) / 2 : expand (b:l)

-- c.
areas :: Fractional a => (a -> a) -> [a] -> a
areas f l = sum $ zipWith (area f) l $ tail l

-- d.
intervals :: [Double] -> [[Double]]
intervals = build expand

aproxI :: (Double -> Double) -> [Double] -> [Double]
aproxI f l = map (areas f) $ intervals l

integral :: (Double -> Double) -> [Double] -> Double
integral f = select 0.0001 . aproxI f


-- functii --
g :: Double -> Double
g = (*) 2.0 

f x = 2 * x ^ 2 - 4 * x + 1.0

f' x = 4 * x - 4.0-}