 --10:25

 -- (1p) 1. Scrieti in Prolog o clauza p(X):- … care se poate satisface 
 -- o singura data, indiferent ce alte clauze ii urmeaza.

 -- p(X) :- q(X), !, r(X).
 -- p(X) :- !


 -- (1p) 2. Scrieti o functie in Haskell care primeste o valoare k, 
 -- o lista, si intoarce toate elementele de pe pozitii multipli de k.
 -- Exemplu: f 3 “Cursuldepp” = “Csdp”

s1 :: Int -> [a] -> [a]
s1 _ [] = []
s1 0 l = l
s1 k l = op 0 l
      where
        op :: Int -> [a] -> [a]
        op _ [] = []
        op i l | i `mod` k == 0 = (head (drop i l)) : op (i+1) l
               | i >= length(l) = []
               | otherwise = op (i+1) l


{-
(2p) 3. Definiti un TDA care codifica porti logice. O poarta poate fi de 3 feluri:

    SI, SAU - avand exact doua intrari (alte porti logice), si
    INPUT - avand o intrare - un boolean

Implementati o functie care primeste o astfel de poarta si intoarce valoarea ei. 
-}

data Poarta = Input Bool | SI Poarta Poarta | SAU Poarta Poarta

s2 :: Poarta -> Bool
s2 (Input a) = a
s2 (SI a b) = (s2 a) && (s2 b)
s2 (SAU a b) = (s2 a) || (s2 b)


-- (2p) 4. Scrieti o functie care intoarce o lista cu toate elementele de deasupra diagonalei principale.
upperDiagList :: [[Int]] -> [[Int]]
upperDiagList [[]] = []
upperDiagList l = (getAboveDiags 0 l)
    where
        getAboveDiags :: Int -> [[Int]] -> [[Int]]
        getAboveDiags _ [[]] = []
        getAboveDiags _ [] = []
        getAboveDiags i (line:mat) = (drop i line) : getAboveDiags (i+1) mat



--10:50

--1 Exemplu de lambda in care var x este libera desi apare legata
-- (Lx.x x)

--2 functie care primeste o lista de intregi si adauga la fiecare valoarea
-- pozitiei din lista. Ex: f [5,4,2,1] = [5,5,4,4]

sub1 :: [Int] -> [Int]
sub1 [] = []
sub1 l = addIndex l 0
      where 
        addIndex [] _ = []
        addIndex (x:xs) i = (x + i) : addIndex xs (i + 1) 

--3 arbore binar ale carui nod stocheaza :
-- un intreg rando, suma tuturor val copiilor 
-- implementati o functie care primeste un sub arbore drept, unul stang, un intreg
-- si construieste un nou nod cu proprietatea descrisa mai sus 

data BTree = Null | Node BTree Int Int BTree deriving Show

sub2 :: BTree -> BTree -> Int -> BTree
sub2 Null Null i = Node Null i 0 Null
sub2 Null (Node l i s r) ind = Node (Node l i s r) ind (s + i) Null
sub2 (Node l i s r) Null ind = Node Null ind (s + i) (Node l i s r)
sub2 (Node l2 i2 s2 r2) (Node l1 i1 s1 r1) ind = Node (Node l1 i1 s1 r1) ind (s1 + s2 + i1 + i2) (Node l2 i2 s2 r2)


--4 functie care primeste doua liste de perechi nume, nota, fiecare nume fiind 
-- unic, si le combina intr-o singura lista de perechi:
-- -fiecare nume apare o singura data
-- -daca exista doua nume cu note diferite in cel mult doua liste, se pastreaza
-- nota din prima lista.
-- Ex combine [("M",1),("G",2)] [("H",3),("M",4)] = [("M",1),("G"),("H",3)]

checkIfInList :: [(String, Int)] -> (String, Int) -> Bool
checkIfInList [] _ = False
checkIfInList (x:xs) y | (fst x) == (fst y) = True
                       | otherwise = checkIfInList xs y

addToEnd :: a -> [a] -> [a]
addToEnd x [] = [x]
addToEnd x l = reverse (x : reverse l) 

sub3 :: [(String, Int)] -> [(String, Int)] -> [(String, Int)]
sub3 [] x = x
sub3 x [] = x
sub3 l (y:ys) | (checkIfInList l y) == False = sub3 l (addToEnd y l)
              | otherwise = sub3 l ys