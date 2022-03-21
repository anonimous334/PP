import Data.List

--1
func1 x = 5


--2
func2 x y = x


--3
func3 :: Bool -> Bool -> Bool
func3 x y = x && y


--4
-- :t func1, :t func2
-- func2 :: p1 -> p2 -> p1
-- func3 :: Bool -> Bool -> Bool


--5
f5 :: Bool -> a -> a -> a
f5 True x _ = x
f5 _ _ y = y


--6
fmax x y = if x > y then x else y

f6 :: Integer -> Integer -> Integer -> Integer 
f6 x y z = fmax x (fmax y z)


--7
f7 x y z = if x then y else z
--f7 :: Bool -> p -> p -> p


--8 
f8 x y z
      | x == True = y 
      | otherwise = z
--f8 :: Bool -> p -> p -> p

--9
f9 x y z = if x > y && x > z then x
    else if y > z && y > z then y
        else z


--10
-- :t [1,2,3] 
--    [1,2,3] :: Num a  => [a]
-- da
-- nu (functie scrisa gresit)


--11
f11 x y z = head(tail(tail(sort[x,y,z])))


--12
f12 l = reverse(l)

--12 fara reverse
f12_ [] = []
f12_ (x:xs) = f12 xs ++ [x]


--13
f13 l | head(tail(tail(f12 l))) `mod` 2 /= 0 = True 
      | otherwise = False


--14
f14 [] = 0
f14 (x:xs) = x + f14 (xs)


--15
f15 [] = True
f15 (x:xs) = if x == True then f15 xs else False


--16
f16 [] = []
f16 (x:xs) | x `mod` 2 == 1 = x : f16 xs 
           | otherwise = f16 xs


--17
f17 [] = []
f17 (x:xs) | x == False = 0 : f17 xs
           | otherwise = 1 : f17 xs


--18
-- f :: [[Integer]] -> [Bool]
--     f [] = []
--     f l = (g (head l)):(f (tail l))
--         where
--             g [] = True
--             g l = h (tail l)
--             h [] = True
--             h l = False 
--
--Ia o lista de liste, verifica numarul de elemente ale fiecareia;
--returneaza o lista de Bool-uri; pentru listele cu mai mult de un element
--intoarce false, altfel true 


--19
f19 [] = 0
f19 (x:xs) | x == True = 1 + f19 xs
           | otherwise = f19 xs

--20
insert_ x [] = [x]
insert_ x (y:ys) = if x < y then x:y:ys else y : insert_ x ys

inssort :: [Integer] -> [Integer]
inssort [] = []
inssort [x] = [x]
inssort (x:xs) = insert_ x (inssort xs)

