import Data.List
-- lista de elemente in care facem x1+xn, x2+xn-1, ....
--aux_f l = tail(take ((length l) - 1) l)

capeteSum :: [Integer] -> [Integer]
capeteSum [] = []
capeteSum l = (head(l)+ head(reverse(l))):capeteSum(aux_f l) 
capeteSum x = x

-- un arbore in care nodurile sunt caractere sau siruri de caractere

data MyTree = Gol |
              CNode Char MyTree MyTree |
              Snode [Char] MyTree MyTree

f :: MyTree -> Char -> Bool
f Gol _ = False
f (CNode c l r) ch = c == ch || (f l ch) || (f r ch)
f (Snode s l r) c = c `elem` s || (f l c) || (f r c)

-- pentru tip generic

data MyTree a = Gol |
				Nod a (MyTree a) (MyTree a)

-- f "matei" = "mae"
f :: [Char] -> [Char]
f (x:y:xs)
	| x `elem` "aeiou" = x:f(tail xs)
	| xs == [] = xs
	| otherwise = x:(f xs)

-- un numar natural si intoarce lista divizorilor lui

f :: Integer -> [Integer]
f x = filter (\e -> x `mod` e == 0) [1..x]

-- o lista si un k si intoarce toate elementele de pe pozitii multiplu de k

g :: [Int] -> Int -> [Int] -- l !! index

g [] _ = []
g l k = (head l) : (g (drop k l) k)

-- definiti un tda ce defineste o poarta si-sau-input si functie pentru 

data PL = Imp Bool | AND PL PL | OR PL PL
eval :: PL -> Bool
eval (Imp x) = x
eval (AND a b) = (eval a) && (eval B)
eval (OR a b) = (eval a) || (eval b)

-- alt ex
f :: [Char] -> [Int]
--f "ada" = [0,2]
f str = reverse $ foldl (\acc x -> if (str !! x) `elem` "aeiouAEIOU" then x:acc else acc) 0 [0 .. (length str - 1)]

f2 str = filter (\x -> str !! x ) `elem` "..." [...]

-- k linii si n coloane

f :: [Char] -> Integer -> [[Char]] -- replicate :: Int -> a -> [a]
f l k = map (\x -> replicate k x) l -- map(replicate k) l
myReplicate 0 _ = []
myReplicate k e = e:myReplicate(k-1) e)

-- funcite cu lista de intregi si adauga la fiecare element pozitia lui din lista

f :: [Integer] -> [Integer]
f [] = []
f l = zipWith(+) l [0..(length l-1)]

-- tda care codifica persoane: N P an sau CNP, sa se scrie o functie care intoarce toate persoanele de dupa 1990

data Pers = Tip1 String String Integer | Tip2 String -- CNP 6010320420020 Ada
aux :: Pers -> Bool
aux (Tip1 _ _ an) = an > 1990
aux (Tip2 str) = 
    where 
          an = (read $ tail $ take 3 str) : Integer
          | an <= 22 = true
          | an >= 90 = true
          | otherwise = false

f :: [Pers] -> [Pers]
f l = filter(aux) l

--  perechi (nume, nota) combina intr-o singura lista in care avem nume unic, se pastreaza nota din prima pereche care apare si intoarce 
f l = reverse $ foldl (\acc (x,y) -> if x `elem` (map(fst) acc) then acc else (x,y):acc) [] l

-- polinom cu an, an-1, ..., a0 si un x si verificam daca x radacina polinomului
aux [] _ _ = 0
aux (a:as) x xind = 
    (aux as x (xs*xind)) + a * xind

f :: [Integer] -> Integer -> Bool
f l x = aux(reverse l) x 1 == 0