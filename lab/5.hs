
-- 1
myCompare :: Int -> Int -> Ordering
myCompare x y | x > y = GT
              | x == y = EQ
              | otherwise = LT


-- 2
insert :: Int -> [Int] -> [Int]
insert a [] = [a]
insert a (x:xs) | a < x = a : x : xs
                | otherwise = x : insert a xs


--3 
insertSort :: [Int] -> [Int]
insertSort = foldr insert []


-- 4
insertBy :: (Int -> Int -> Ordering) -> Int -> [Int] -> [Int]
insertBy _ x [] = [x]
insertBy op x (l:ls) | op x l /= GT = x : l : ls
                     | otherwise = l : insertBy op x ls


-- 5
insertSortBy :: (Int -> Int -> Ordering) -> [Int] -> [Int]
insertSortBy _ [] = []
insertSortBy op l = foldr (insertBy op) [] l


data Point = Point Float Float deriving Show

pointToPair :: Point -> (Float, Float)
pointToPair (Point x y) = (x, y)


-- 6
distance :: Point -> Point -> Float
distance (Point ax ay) (Point bx by) = sqrt ( (ax - bx)**2 + (ay - by)**2 )


-- 7
collinear :: Point -> Point -> Point -> Bool
collinear (Point ax ay) (Point bx by) (Point cx cy) = 
    abs(( ax*(by - cy) + bx*(cy - ay) + cx*(ay - by) )/2) == 0


data Natural = Zero | Succ Natural deriving Show

-- 8
add :: Natural -> Natural -> Natural
add Zero Zero = Zero
add Zero a = a
add a Zero = a
add (Succ a) b = Succ (add a b)

-- 9
mul :: Natural -> Natural -> Natural
mul Zero Zero = Zero
mul Zero a = Zero
mul a Zero = Zero
mul (Succ a) b = add (mul a b) b


data IList = IVoid | ICons Int IList
data List a = Void | Cons a (List a) deriving Show


-- 10
myLength :: List a -> Int
myLength Void = 0;
myLength (Cons x xs) = 1 + myLength xs


-- 11
toHaskell :: List a -> [a]
toHaskell Void = []
toHaskell (Cons x xs) = x : toHaskell xs


-- 12
data Tree a = TVoid | Node a (Tree a) (Tree a) deriving Show


-- 13
height :: Tree a -> Int
height TVoid = 0
height (Node x l r) = 1 + max (height l) (height r)


-- 14
size :: Tree a -> Int
size TVoid = 0
size (Node x l r) = 1 + (size l) + (size r)


-- 15
mirror :: Tree a -> Tree a
mirror TVoid = TVoid
mirror (Node x l r) = Node x (mirror r) (mirror l)


-- 16
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap op TVoid = TVoid
treeMap op (Node x l r) = Node (op x) (treeMap op l) (treeMap op r)


-- 17
flatten :: Tree a -> [a]
flatten TVoid = []
flatten (Node x l r) = x : (flatten l) ++ (flatten r)


data Student = Student String String [Float] deriving Show

-- 18
avg :: Student -> Float
avg (Student _ _ l) = sum l / fromIntegral (length l)


-- 19
studComp :: Student -> Student -> Ordering
studComp x y | avg x > avg y = GT
             | avg x == avg y = EQ
             | otherwise = LT


-- 20
highestAverage :: [Student] -> String
highestAverage l = op $ head $ reverse $ insertSortAvg l
                 where op (Student name surname _) = name ++ " " ++ surname

sortAvg :: Student -> [Student] -> [Student]
sortAvg s [] = [s]
sortAvg s (x:xs) | studComp s x == GT = x : sortAvg s xs
                 | otherwise = s : x : xs


insertSortAvg :: [Student] -> [Student]
insertSortAvg = foldr sortAvg [] 


data AExpr = Const Int | Var String | Add AExpr AExpr | Mul AExpr AExpr
data BExpr = Eq AExpr AExpr | Not BExpr | Gt AExpr AExpr
type Context = [(String, Int)]

-- 21
search :: Context -> 
