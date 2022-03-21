import Data.Char
import Data.Bits

--46
-- f x y = x + y
-- f x = \y -> x + y
-- f = \x -> \y -> x + y
-- f = \x y -> x + y

l = ["matei@gmail.com", "mihai@gmail.com", "tEst@mail.com", "email@email.com", "short@ax.ro"]

lenL = foldr (\x y -> (x, length x):y) [] l


--1
rem_upper :: [[Char]] -> [[Char]]
rem_upper = \x -> map (map Data.Char.toLower) x


--1.2
longer :: Int -> [String] -> [String]
longer = \x -> \y -> filter(not . null)(map(\z -> if length z <= x then z else "" ) y)


-- longerx :: Int -> [String] -> [String]
-- longerx = \x -> \y -> filter(\z -> length z <= x) y


--1.3
howmany :: [String] -> Int
howmany = foldl(\acc x -> if length x > 12 then acc + 1 else acc) 0


-- cu foldr
-- howmany :: [String] -> Int
-- howmany = foldr(\x -> \acc -> if length x > 12 then acc + 1 else acc) 0


splitr :: Char -> String -> [String]
splitr sep = foldr op []
       where
        op c [] | c == sep = []
                | otherwise = [[c]]
        op c (x:xs) | c == sep = "":(x:xs)
                    | otherwise = (c:x):xs 

list2tuple :: [String] -> (String,String)
list2tuple x = (head(x),(head(tail(x))))

-- 1.4
names_emails x = unzip (map list2tuple (map (splitr '@') x))
-- transform lista in lista de perechi, dupa fac unzip


lnames_emails :: [String] -> [[String]]
lnames_emails = map op
      where
        op = foldr op2 [[]]
        op2 c ([]:t) | c == '@' = []:t
                     | otherwise = [c]:t
        op2 c (x:t) | c == '@' = []:t
                    | otherwise = (c:x):t


-- 1.5
domains :: [String] -> [String]
domains = \x ->filter(not . null)(map (\y -> if y == head(x) then [] else y ) x)


-- 1.6
splitByL :: Char -> String -> [String]
splitByL sep = reverse . map reverse . foldl op []
       where
        op [] c | c == sep = []
                | otherwise = [[c]]
        op (x:xs) c | c == sep = "":(x:xs)
                    | otherwise = (c:x):xs 


-- 1.7
domain :: [String] -> [String]
domain = map ( head . splitByL '.' . head . tail . splitByL '@') 


-- 2.1
s1 1 = True
s1 2 = True
s1 _ = False
 
s2 x = mod x 2 == 0
 
s3 _ = False

mem :: (Integer -> Bool) -> Integer -> Bool
mem set = set
-- mem = \s x -> if (s x) == True then True else False


-- 2.2
sp2 x = x .&. (x-1) == 0

-- 2.3
sNat 0 = True
sNat x = x > 0 && x == fromInteger(abs ( round (x) ) )


-- 2.4