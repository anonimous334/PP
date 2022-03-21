import Data.List
import Data.Maybe
import Data.Char
--Duminica
--10-10:25

-- (L/P) Scrieti primul si al doilea pas din evaluarea normala/aplicativa a expresiei 
-- ((λx.(x x) λx.(x x))(λx.x λx.x)).

--normala:
{-
((λx.(x x) λx.(x x))(λx.x λx.x)) =>n
((λx.(x x) λx.(x x))(λx.x λx.x)) 
((λx.(x x) λx.(x x))(λx.x λx.x))

-}

--aplicativa
{-
((λx.(x x) λx.(x x))(λx.x λx.x)) =>a
((λx.(x x) λx.(x x))(λx.x))
((λx.(x x) λx.(x x))(λx.x))

-}

-- (E1) Scrieti o functie in Haskell care primeste lista x1, x2 ... xn si intoarce lista:
-- x1 + xn, x2 + xn-1, x3 + xn-2...

-- e1 :: [Int] -> [Int]
-- e1 [] = []
-- e1 l | (length l) /= 1 = (head l + head ( reverse l)) : e1 (tail $ reverse $ tail $ reverse l)
--      | otherwise = head l : []

-- -- (E2) Definiti un arbore binar, in care nodurile pot fi siruri de caractere 
-- -- sau doar caractere (insa nu altceva). 
-- -- Implementati o functie care ia un arbore, un caracter, 
-- -- si verifica daca caracterul apare in arbore.

-- data BTree a = BNull | BNode [Char] (BTree a) (BTree a) deriving Show

-- e2 :: BTree a -> Char -> Bool
-- e2 (BNode s BNull BNull) c = c `elem` s
-- e2 (BNode s l _) c = if c `elem` s then True else e2 l c
-- e2 (BNode s _ r) c = if c `elem` s then True else e2 r c
-- e2 (BNode s l r) c = if c `elem` s then True else (||) (e2 l c) (e2 r c)


-- -- (E3) Scrieti o functie primeste o tabla de sah
-- -- (codificata ca o matrice patratica). Stiind ca tabla 
-- -- poate contine doar ture (turnuri), functia trebuie 
-- -- sa intoarca True, daca exista doua ture care se ataca reciproc.

-- -- line :: [Char] -> Int
-- -- line [] = 0
-- -- line (x:xs) = if x == 't' then 1 + line xs else 0 + line xs 

-- line :: [[Int]] -> [Int]
-- line [] = []
-- line (x:xs) = sum x : line xs

-- e3 :: [[Int]] -> Bool
-- e3 t = (||) (op1 s) (op2 s)
--  where
--      s = line t
--      op1 [] = False
--      op1 (x:xs) = if x > 1 then True else op1 xs
--      count [] = 0
--      count (x:xs) = if x > 0 then 1 + count xs else 0 + count xs
--      op2 l = count l > 1

--Duminica
--10:25-10:50

-- (Scrieti in Prolog o clauza echivalenta 
-- cu propozitia "forall X. ~p(X) V q(X) V ~r(X)”.

-- q(x) :- r(x) , p(x)

-- (E1) Scrieti o functie in Haskell care primeste o lista de siruri 
-- de caractere si intoarce o lista cu indicii acelor siruri care incep
-- cu majuscula. Exemplu: f ["Matei", "dan", "andrei", "Mihai"] = [0,3]    

-- e1 :: [String] -> [Int]
-- e1 l = map (\x -> fromJust (elemIndex x l) ) newlist
--    where 
--      newlist = filter (\x -> isUpper (head x) == True) l

-- (E2) Definiti un TDA care codifica cele patru baze azotate ce compun
-- o secventa de ADN: (A, T, C, G). Scrieti o functie care verifica
-- daca o lista de ADN este complementul alteia. 
-- (A este complementul lui T iar C - al lui G). 
-- Hint: ce ingrediente sunt necesare pentru a asigura comparatia?

-- data Atom = A | C | G | T

-- comp :: Atom -> Atom -> Bool
-- comp A T = True
-- comp C G = True
-- comp T A = True
-- comp G C = True
-- comp _ _ = False

-- e2 :: [Atom] -> [Atom] -> Bool
-- e2 [] [] = True
-- e2 (x:xs) (y:ys) | comp x y == True = e2 xs ys
--                  | otherwise = False

-- (E3) Scrieti o functie care primeste o imagine (matrice de caractere),
-- si o valoare 'k', si construieste repetitia imaginii de k ori, 
-- atat pe linii si pe coloane. Daca [“#”] este o imagine 1×1 atunci
-- repetitia ei de 3 ori este: [“###”,“###”,“###”].

-- lat :: [String] -> Int -> [String]
-- lat img 1 = img
-- lat img k = img ++ lat img (k-1)

-- e3 :: [String] -> Int -> [String]
-- e3 img k = lat [(concat (lat img k))] k

--Duminica
--10:50-11:15

-- (L/P) 
-- Explicati care sunt variabilele libere din expresia 
-- (λx.x (y λy.(y x))).
--        |

-- (E1) Scrieti o functie in Haskell care primeste un sir de caractere 
-- lowercase, si elimina din acesta toate caracterele 
-- care sunt precedate de o vocala. 
-- Exemplu: f “matei” = “mae” .

-- e1 :: String -> String
-- e1 [] = []
-- e1 (x:y:xs) | x == 'a' = x : e1 xs
--             | x == 'e' = x : e1 xs
--             | x == 'i' = x : e1 xs
--             | x == 'o' = x : e1 xs
--             | x == 'u' = x : e1 xs
--             | otherwise = x : e1 (y:xs)

-- data Logic = SI Logic Logic | SAU Logic Logic | INPUT Bool

-- e2 :: Logic -> Bool
-- e2 (INPUT b) = b
-- e2 (SI b1 b2) = (&&) (e2 b1) (e2 b2)
-- e2 (SAU b1 b2) = (||) (e2 b1) (e2 b2)

-- (E3) Scrieti o functie care primeste un numar natural si 
-- intoarce lista tuturor divizorilor lui.

-- e3 :: Int -> [Int]
-- e3 x = op 1 x
--     where op k x | x `mod` k == 0 = k : op (k+1) x
--                  | k > x = []
--                  | otherwise = op (k+1) x


-- Luni 10:25

-- (1p) 1. Scrieti in Prolog o clauza p(X):- … care se poate satisface o singura data, indiferent ce alte clauze ii urmeaza.
--   p(X) :- !.


-- (1p) 2. 2. Scrieti o functie in Haskell care primeste o valoare k, 
-- o lista, si intoarce toate elementele de pe pozitii multipli de k.
--  Exemplu: f 3 “Cursuldepp” --= “Csdp”

-- e1 :: Int -> String -> String
-- e1 _ [] = []
-- e1 k s = op 0 s
--     where 
--       op _ [] = []
--       op i l | mod i k == 0 = head (drop i l) : op (i + 1) l
--              | i > length l = []
--              | otherwise = op (i + 1) l

{-
(2p) 3. Definiti un TDA care codifica porti logice. O poarta poate fi de 3 feluri:

    SI, SAU - avand exact doua intrari (alte porti logice), si
    INPUT - avand o intrare - un boolean

Implementati o functie care primeste o astfel de poarta si intoarce valoarea ei. 
-}

-- (2p) 4. Scrieti o functie care intoarce o lista cu toate elementele 
-- de deasupra diagonalei principale. 

-- e3 :: [[Int]] -> [[Int]]
-- e3 [[]] = []
-- e3 m = map (\r -> drop (fromJust (elemIndex r m) +1) r ) m 


-- Luni 11:40-12:05 part 1

-- (1p) 1. Scrieti in Prolog un predicat care calculeaza
-- suma elementelor dintr-o lista.

-- sum([], 0).
-- sum([H1|H2|T], R) :- sum ([H2|T], R + H)

-- (1p)Scrieti o functie in Haskell care primeste o lista
-- de perechi de intregi, si intoarce o pereche cu primele,
-- respectiv ultimele elemente. 

-- e1 :: [(Int, Int)] -> ([Int],[Int])
-- e1 [] = ([],[])
-- e1 l = (prim l, second l)
--     where 
--       prim :: [(Int, Int)] -> [Int]
--       prim [] = []
--       prim (x:xs) = fst x : prim xs
--       second :: [(Int, Int)] -> [Int]
--       second [] = []
--       second (x:xs) = snd x : second xs


{-
(2p) Definiti un TDA care codifica informatii despre persoane,
 in doua formate diferite:

-- Nume si prenume (un String), si anul nasterii
-- (un intreg intre 1900 si 1999)
    
-- CNP (un String).

Scrieti o functie care primeste o lista de persoane si
intoarce o lista cu persoanele nascute dupa 1990. 
-}

-- data Persoana = NPV (String, Int) | CNP String deriving Show

-- e2 :: [Persoana] -> [Persoana]
-- e2 [] = []
-- e2 l = filter op l 
--      where op (NPV (s, an)) = an > 1990
--            op (CNP s) = s !! 1 == '9' && s !! 2 /= '0'

{-
(2p) 4. Scrieti o functie care primeste o imagine codificata ca o 
matrice, si doua perechi de coordonate x,y si z,w care codifica 
un dreptunghi avand coltul din stanga sus la pozitia x,y si coltul 
din dreapta la pozitia z,w, si intoarce portiunea din imagine 
delimitata de dreptunghi. Exemplu:

######
###..#
######

1,1  2,4

##..
####

["######","###..#","######"]-}

-- e3 :: [String] -> (Int, Int) -> (Int, Int) -> [String]
-- e3 img (x,y) (z,w) = map (\r -> take w r) (map (\r -> drop y r) aux)
--     where aux = take z $ drop x img


-- Luni 11:40-12:05 part 2

-- (1p) 1. Dati un exemplu de lambda expresie 
-- pentru care evaluarea aplicativa se termina, insa cea normala cicleaza.

-- nu exista, daca cicleaza normal, cicleaza si aplicativ



-- -- (1p) 2. Scrieti o functie in Haskell care primeste un string si intoarce 
-- pozitiile tuturor vocalelor din acesta. Exemplu: f “Matei” = [1,3,4].

-- e1 :: String -> [Int]
-- e1 [] = []
-- e1 l = op 0 l
--      where
--         op _ [] = []
--         op ind (x:xs) | x == 'a' = ind : op (ind+1) xs
--                       | x == 'e' = ind : op (ind+1) xs
--                       | x == 'i' = ind : op (ind+1) xs
--                       | x == 'o' = ind : op (ind+1) xs
--                       | x == 'u' = ind : op (ind+1) xs
--                       | otherwise = op (ind+1) xs


{-
(2p) 3. Definiti un TDA care codifica piese de puzzle avand una 
din cele trei configuratii de mai jos.
 
https://ocw.cs.pub.ro/ppcarte/lib/exe/fetch.php?cache=&w=720&h=405&tok=f8c61d&media=pp:slide1.jpeg
 
 
Scrieti o functie care primeste o lista de piese si verifica 
daca acestea pot fi combinate una in continuarea celeilalte 
(exact asa cum apar in lista)
-}

-- data Piece = P1 | P2 | P3

-- e2 :: [Piece] -> Bool
-- e2 [] = True
-- e2 [x] = True
-- e2 (x:y:xs) | (x == P1) && (y == P2) = e2 (y:xs)
--             | (x == P2) && (y == P3) = e2 (y:xs)
--             | (x == P3) && (y == P1) = e2 (y:xs)
--             | otherwise = False

{-
(2p) 4. Scrieti o functie care primeste ca parametru un polinom 
an * X^n + a{n-1} * X^{n-1} … + a1 * X + a0 
codificat ca o lista an, an-1, …, a1, a0 si o valoare x, 
si verifica daca x este radacina a polinomului.
-}

-- e3 :: [Int] -> Int -> Bool
-- e3 [] _ = False
-- e3 pol x = (op pol (length pol) x) == 0
--         where
--           op :: [Int] -> Int -> Int -> Int
--           op [] _ _ = 0
--           op (y:ys) n x = y * x^(n-1) + (op ys (n-1) x)


-- Luni 12:20-??? (subiectul meu)

-- (1p) 1. Scrieti primul si al doilea pas 
-- din evaluarea normala/aplicativa a expresiei

-- (λx.(x x) (λx.(x x) (λx.x λx.x))). 
{- norm:
  ((λx.(x x) (λx.x λx.x)) (λx.(x x) (λx.x λx.x)))
  ((λx.x λx.x) (λx.x λx.x)) (λx.(x x) (λx.x λx.x)))

  apl:
  (λx.(x x) (λx.(x x) λx.x ))
  (λx.(x x) (λx.x λx.x))
-}

-- (1p) 2. Scrieti o functie care primeste o lista de valori 
-- x1,x2,…xn si un intreg k si construieste matricea cu k linii si n
-- coloane, in care linia i are valorile xi,…xi - de k ori

{-
[1,88,9] 2

1 1 1
88 88 88
-}

-- e1 :: [Int] -> Int -> [[Int]]
-- e1 l k = op l k (length l) 0
--     where 
--         op [] _ _ _ = []
--         op (x:xs) k n ind | ind < k = replicate n x : op xs k n (ind + 1)
--                           | otherwise = []


{-
(2p) 3. Definiti un TDA care codifica o secventa de becuri, 
legate in serie, fiecare avand o culoare (R,G,B), doar atunci cand 
este pornit. Scrieti o functie care primeste o secventa de becuri, 
si intoarce (daca exista), cea mai lunga sub-secventa de becuri 
nefunctionale din cauza unui bec oprit. 
-}

-- data Culoare = R | G | B deriving Show
-- data Becuri = On Culoare | Off
-- data Sec = Sec [Becuri]

-- onoff :: Becuri -> Bool
-- onoff (On _) = True
-- onoff (Off _) = False

-- e2 :: Sec -> Sec
-- e2 (Sec (x:xs)) | onoff x == False = op 0 xs
--                 | otherwise = e2 (Sec xs)
--     where
--         op ind (x:xs)

{- (2p) 4. Scrieti o functie care primeste o imagine teoretic
 infinita (liniile si coloanele pot fi arbitrar de mari), 
 si construieste lista f0, f1, f2, …, fn, unde fiecare functie 
 fi(k) intoarce pozitia k de pe linia i. -}

-- e3 :: [String] -> [Int -> Char]
-- e3 img = map (\ln i -> ln !! i) img


-- Marti
{-
  Dati un exemplu de lambda-expresie in care variabila x este libera desi ea
    are o aparitie legata.
-}

-- (Ly.Lx.x x)

{-
    E1. Scrieti o functie in Haskell care primeste o lista de intregi si adauga
    la fiecare, valoarea pozitiei din lista, Exemplu: f [5,4,2,1] = [5,5,4,4]
-}
-- e2 :: [Int] -> [Int]
-- e2 [] = []
-- e2 l = op l 0
--     where 
--       op [] _ = []
--       op (x:xs) ind = x + ind : op xs (ind + 1)

{-
E2. Definiti un arbore binar ale carui noduri stocheaza: un intreg oarecare,
impreuna cu suma tuturor valorilor copiilor. Implementati o functie care
primeste un sub-arbore drept, unul stang, un intreg, si construieste un
nod nou cu proprietatea descrisa mai sus.
-}

-- data BTree = Void | Node Int Int deriving (Show, Eq)

-- e3 :: BTree -> BTree -> Int -> BTree
-- e3 (Node i1 s1) (Node i2 s2) i = Node (i + i1 + i2) (s1 + s2)


{-
E3. Scrieti o functie care primeste doua liste de perechi de nume, nota,
fiecare nume fiind unic, si le combina intr-o singura lista de perechi
in care: fiecare nume apare o singura data. Daca exista 2 nume cu note
diferite in cele doua liste, se pastreaza nota din prima lista.
Exemplu: combine [("M", 1), ("G", 2)] [("H", 1), ("M", 2)] =
[("M", 1), ("G", 2), ("H", 1)]
-}


-- rm :: String -> [(String, Int)] -> [(String, Int)]
-- rm _ [] = []
-- rm str (x:xs) = if fst x == str then xs else x : (rm str xs)
-- conc :: [(String, Int)] -> [(String, Int)] -> [(String, Int)]
-- conc [] [] = []
-- conc (x:xs) l2@(y:ys)
--     | fst x `elem` (map (fst) (l2)) = x : (conc xs (rm (fst x) l2))
--     | otherwise = x : y : (conc xs ys)
