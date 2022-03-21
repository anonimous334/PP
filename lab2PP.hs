module Lab2 where
import Data.Char
import Data.List

--Pattern Matching
--1.1
f11 [] = 0
f11 (x:xs) = 1 + (f11 xs)


--1.2
f12 :: [[Integer]] -> [Integer]
f12 [] = []
f12 (x:xs) = x ++ f12 (xs)


--1.3
f13 :: [[Integer]] -> [Integer]
f13 [] = []
f13 (x:[]) = x
f13 ([]:x) = f13 x
f13 ((x:xs):l) = x : f13 (xs:l)


--1.4
f14 :: [Integer] -> [Integer]
f14 [] = []
f14 [x] = [x]
f14 (x:xs) = x : (f14(rm x xs))
    where 
        rm :: Integer -> [Integer] -> [Integer]
        rm x [] = []
        rm x (y:ys) | x /= y = y : rm x ys
                    | otherwise = rm x ys


--Types in Haskell
--2.1
-- f21 (x:y:z:w:l) = w
-- f21 _ = 0
-- f21 = "f :: Num p => [p] -> p"
-- :t x,y,z,w :: Num p => p
-- :t l :: Num a => [a]

-- f21a (x:y:z:w:l) = w
-- :t x,y,z,w,l :: a
-- f21a :: [a] -> a

-- nu putem folosi f21 pentru siruri



--2.2
-- f22 x y = (x,y)
-- :t f22
f22 = "f :: a -> b -> (a, b)"


--2.3
-- f23 'a' _ = []
-- f23 x y = x:y
-- :t f23
f23 = "f :: Char -> [Char] -> [Char]"


--2.4
-- f24 "321CB" [("321CB", ["Matei", "Andrei", "Mihai"]), ("322CB",["George", "Matei"])] = ["Matei", "Mihai"]
-- :t f24
f24 = "f :: [Char] -> [([Char], [[Char]])] -> [[Char]]"



--3.1
f31 :: [[Char]] -> [[Char]]
f31 [] = []
f31 ((x:xs):ys) = (Data.Char.toUpper x : xs) : f31 ys

--3.2
f32 :: [[Char]] -> [[Char]]
f32 [] = []
f32 (x:xs) = map Data.Char.toUpper x : f32 xs

-- Strings in Haskell
--3.3
f33 :: [Char] -> [Char] -> Integer
f33 h n = addTrues(findNeedle h n)
    where 
        findNeedle :: [Char] -> [Char] -> [Bool]
        findNeedle h n = map (isPrefixOf n) (tails h)
        addTrues :: [Bool] -> Integer
        addTrues [] = 0
        addTrues (x:xs) | x == True = 1 + addTrues xs
                        | otherwise = addTrues xs


--3.4
f34 :: [Char] -> [([Char], [[Char]])] -> [[Char]]
f34 gr (x:xs) | fst(x) == gr = filtM (snd(x))
               | otherwise = f34 gr xs
    where 
        filtM :: [[Char]] -> [[Char]]
        filtM [] = []
        filtM (x:xs) | head(x) == 'M' = x : filtM xs
             | otherwise = filtM xs
-- f ia ca parametru un [Char] si o lista de perechi ([Char], [[Char]])
-- cauta perechile care au grupa respeciva (Ex: 321CB), si afiseaza numai
-- studentii a caror nume incep cu 'M'



--3.5
-- f35 ["Matei", "Mihai"] ["Popovici","Dumitru"] = [("Matei","Popovici"), ("Mihai","Dumitru")]
f35 :: [[Char]] -> [[Char]] -> [([Char], [Char])]
f35 x y = (head(x),head(y)) : (head(tail(x)),head(tail(y))) : []


--3.6
-- f36 ["Matei", "Mihai"] ["Popovici","Dumitru"] = ["MPopovici", "MDumitru"]
f36 :: [[Char]] -> [[Char]] -> [[Char]]
f36 [] [] = []
f36 (x:xs) (y:ys) = (head(x) : y) : f36 xs ys 


--3.7
-- f37 [("Dan Matei Popovici",9),("Mihai",4),("Andrei Alex",6)] =
--       [(["Dan", "Matei", "Popovici"],10),(["Andrei,Alex"],6)]
f37 :: [([Char], Integer)] -> [([[Char]], Integer)]
f37 [] = []
f37 l = bonus(filtByGrade l)
    where
        filtByGrade [] = []
        filtByGrade (x:xs) | snd(x) >= 5 = (words (fst(x)), snd(x)) : filtByGrade xs
                           | otherwise = filtByGrade xs
        bonus [] = []
        bonus (x:xs) | length (fst(x)) >= 3 = (fst(x), snd(x) + 1) : bonus xs
                     | otherwise = (fst(x), snd(x)) : bonus xs