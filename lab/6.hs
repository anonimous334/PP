{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}



import           Data.Char

-- Data types
data List a = Null | Cons a (List a)
data BTree a = Void | Node a (BTree a) (BTree a)
data Student = Student {
  first_name :: String,
  last_name  :: String,
  grades     :: [Float]
}
{--
  For Student, we can define it as a 'struct Student' from C.
  first_name will be a "getter" for our first String.
  Haskell helps us by defining 3 simple functions for us to extract each member
  Try :t Student and :t first_name in GHCI and you'll figure it out quickly.
--}


-- We should also have some test data You can also create your own.
tree1 = Node 1 (Node 2 (Node 3 Void Void) Void) (Node 4 Void (Node 5 Void Void))
tree2 = Node 2 (Node 3 (Node 1 Void Void) Void) (Node 5 Void (Node 4 Void Void))  -- same as tree1, but different nodes order
tree3 = Node 1 (Node 2 (Node 3 (Node 7 Void Void) (Node 10 Void Void)) (Node 4 (Node 6 Void Void) (Node 9 Void Void))) (Node 5 (Node 8 Void Void) Void)
tree4 = Node 1.2 (Node 3.4 Void (Node 1.4 Void Void)) (Node 3.0 Void Void) -- and a float tree


list1 = Cons 1 (Cons 2 (Cons 3 (Cons 4 Null)))
list2 = Cons 3 (Cons 4 (Cons 1 (Cons 2 Null)))
-- list3 = Cons list1 list2

student1 = Student "Alex" "Andrei" [4.2,3.0,2.3]
student2 = Student "Matei" "Popovici" [10.0,7.7,9.4]
student3 = Student "Mihai" "Dumitru" [7.7,9.4,10.0]

students = [student1, student2, student3]


-- 0. Adding 'deriving Show' to our data types will print them in a 'default' manner (you can try).
-- Let's print a tree in our way. For this we will enroll BTree in the Show class

-- Because we'll have to also print the value encapsulated in each node,
-- the value must also be "showable". So we will have to add this restriction on 'a'.

-- instance (Show a) => Show (BTree a) where
--   show Void         = ""
--   show (Node v l r) = "<"++(show l)++(show v)++(show r)++">"


-- It's nice, but let's do this more "stylish", just for flexing. We'll create the next showTree function
data Side = LeftSide | RightSide deriving Eq
data Front = Lane | Tab deriving Eq

generateLane :: String
generateLane = "─" ++ (replicate 3 ' ')

generateTab :: String
generateTab = " " ++ (replicate 3 ' ')

generateValue :: (Show a) => BTree a -> Side -> String
generateValue Void childSide =
  (if childSide == LeftSide then "┌" else "┐") ++ (replicate 3 '│') ++ "@" ++ "\n"
generateValue (Node value _ _) childSide =
  (if childSide == LeftSide then "┌" else "┐") ++ (replicate 3 '│') ++ (show value) ++ "\n"

generateNodeText :: (Show a) => [Front] -> Side -> BTree a -> String
generateNodeText fronts childSide node =
  (concat $ map (\frontType -> if frontType == Lane then generateLane else generateTab) fronts)
  ++ (generateValue node childSide)

showTree :: (Show a) => [Front] -> Side -> BTree a -> String
showTree fronts childSide tree@(Void) = generateNodeText fronts childSide tree
showTree fronts childSide node@(Node value left right) =
  if childSide == LeftSide then
    showTree (fronts ++ [Tab]) LeftSide left ++
    generateNodeText fronts childSide node ++
    showTree (fronts ++ [Lane]) RightSide right
  else
    showTree (fronts ++ [Lane]) LeftSide left ++
    generateNodeText fronts childSide node ++
    showTree (fronts ++ [Tab]) RightSide right

transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x      = (map head x) : transpose (map tail x)

splitByR sep string = foldr (\char acc@(currentStr:ls) -> if char == sep then [] : acc else (char : currentStr) : ls) [[]] string


instance (Show a) => Show (BTree a) where
  show Void = ")("
  show tree@(Node value left right) =
    let res = splitByR '\n' $ (showTree [] LeftSide left) ++ (show value) ++ "\n" ++ (showTree [] RightSide right)
    in
      concat $ map (\line -> line ++ "\n") $
      transpose $
      map (\line -> take (max (length res) (maximum $ map length res)) $ line ++ repeat ' ') res


{-

  1. Add List and Student to the Show class. You can print them however you want.
  If you aren't inspired today, you can use the following:
    *. The lists can be the default style [1,2,3].
    *. The student can be something like -> "Studentul: ANDREI Alex-Bogdan = [8.5,6.0,8.7]"
-}
instance Show (Student) where
  --show (Student fn ln grd) = "Studentul: " ++ map toUpper ln ++ ' ' : fn ++ " = " ++ show grd
  show s = "Studentul " ++ map toUpper (last_name s) ++ ' ' : first_name s ++ " = " ++ (show.grades) s

instance (Show a) => Show (List a) where
  show Null = "{}"
  show l = '{' : op l
    where
      op (Cons h Null) = show h ++ "}"
      op (Cons h t) = show h ++ ", " ++ op t


{--
  2. The default '==' that we get from 'deriving Eq' will check if 2 objects are identical.
  For our data, you'll have to provide a custom '==' such that:
    - list1 == list2 = True if both trees have the same elements, but in any order
    - tree1 == tree2 = True if both trees have the same elements, but in any order
    - stud1 == stud2 = True if both students have the same average on their grades
--}
average :: [Float] -> Float
average l = sum l / fromIntegral (length l)

instance Eq (Student) where
  s1 == s2 = average (grades s1) == average (grades s2)

-----------------------------

lengthList :: Num p => List a -> p
lengthList Null = 0
lengthList (Cons _ t) = 1 + lengthList t

removeFirst :: Eq a => a -> List a -> List a
removeFirst e Null = Null
removeFirst e (Cons h t)
  | h == e = t
  | otherwise = Cons h $ removeFirst e t

instance (Eq a) => Eq (List a) where
  l1 == l2 = op l1 l2
    where
      op l1 l2
        | lengthList l1 == lengthList l2 = op2 l1 l2
        | otherwise = False
      op2 Null Null = True
      op2 Null (Cons _ _) = False
      op2 (Cons _ _) Null = False
      op2 (Cons h t) l = op2 t $ removeFirst h l

--------------------

removeFirstBasic :: Eq t => t -> [t] -> [t]
removeFirstBasic e [] = []
removeFirstBasic e (h:t)
  | h == e = t
  | otherwise = (:) h $ removeFirstBasic e t

flatten :: BTree a -> [a]
flatten Void = []
flatten (Node v l r) = flatten l ++ v : flatten r

instance (Eq a) => Eq (BTree a) where
  t1 == t2 = op (flatten t1) $ flatten t2
    where
      op l1 l2
        | length l1 == length l2 = op2 l1 l2
        | otherwise = False
      op2 [] [] = True
      op2 [] (_:_) = False
      op2 (_:_) [] = False
      op2 (h:t) l = op2 t $ removeFirstBasic h l

{--
  3. We would like to use + and * on lists and trees to add/multiply corespondent elements.
  Enroll BTree and List in the Num class to access the + and * functions.
  If 2 lists or trees aren't at the same size, then we should consider unexisting corespondents as Null/Void.
  Also:
    Void + Node = Node
    Void * Node = Void
    same for lists

instance (Num a) => Num (List a) where
  (+) Null Cons = Cons
  (*) Null Cons = Null
--}
instance (Num a) => Num (List a) where
  (+) Null (Cons h t) = (Cons h t)
  (+) (Cons h t) Null = (Cons h t)
  (+) Null Null = Null
  (+) (Cons h1 t1) (Cons h2 t2) = Cons (h1+h2) (t1+t2)
  (*) Null list = Null
  (*) list Null = Null
  (*) Null Null = Null
  (*) (Cons h1 t1) (Cons h2 t2) = Cons (h1*h2) (t1*t2)


instance (Num a) => Num (BTree a) where
  (+) Void (Node a l r) = (Node a l r)
  (+) (Node a l r) Void = (Node a l r)
  (+) Void Void = Void
  (+) (Node a1 l1 r1) (Node a2 l2 r2) = (Node (a1+a2) (l1+l2) (r1+r2))
  (*) Void (Node a l r) = Void
  (*) (Node a l r) Void = Void
  (*) Void Void = Void
  (*) (Node a1 l1 r1) (Node a2 l2 r2) = (Node (a1*a2) (l1*l2) (r1*r2))

{--
  4. Let's sort students now. Add the student to the Ord class and provide implementations for <.
  The criteria will be their grades average, then maximum grade, last_name and first_name alphabetical.

  We will sort them by rankings, stud1 < stud2 if stud1 is better than stud2 by the above criteria.
--}
instance Ord Student where
  stud1 < stud2 | average (grades stud1) < average (grades stud2) = True
                | average (grades stud1) == average (grades stud2) = check_max
                | otherwise = False
              where check_max | maximum (grades stud1) < maximum (grades stud1) = True
                              | maximum (grades stud1) == maximum (grades stud1) = check_last
                              | otherwise = False
                             where check_last | last_name (stud1) < last_name (stud2) = True
                                              | last_name (stud1) == last_name (stud2) = check_first
                                              | otherwise = False
                                             where check_first | first_name (stud1) <= first_name (stud2) = True
                                                               | otherwise = False

{--
  What if we need to create our own classes?
  For a quick example, we would like a class that tells us if a data type is Empty or not.
  We will call this class IsVoid and all types enrolled in this class must implement the isVoid method.

  As you can see, now we can add our data types in the new class, but also Haskell's types.
  The only requirement is that the enrolled type must implement our method.
--}

class IsVoid a where
  isVoid :: a -> Bool

instance IsVoid Bool where
  isVoid False = True
  isVoid True  = False


instance IsVoid (BTree a) where
  isVoid Void = True
  isVoid _    = False

instance IsVoid [a] where
  isVoid [] = True
  isVoid _  = False

instance IsVoid (List a) where
  isVoid Null = True
  isVoid _  = False

{-
  5. Create a class 'Contains b a' that will require a 'contains :: b -> a -> Bool' method which will return True if 'a' is in 'b'.
  Do we need any additional restrictions for a or b? You can still add restrictions
    'class (SomeClass a) => Contains b a where ...'
-}

class Contains b a where
  contains :: b -> a -> Bool

instance Eq a => Contains [a] a where
  contains [] _ = False
  contains (h:t) e
    | h == e = True
    | otherwise = contains t e

--TODO: instance Contains pentru data List definit mai sus

instance Eq a => Contains (List a) a where
  contains Null _ = False
  contains (Cons h t) e 
     | h == e = True
     | otherwise = contains t e 

{-
  6. Create the 'class Size a' which will require the methods 'size' and 'uniqueSize'
     - size = the numbers of elements in 'a'
     - uniqueSize = the number of uniqueElements in 'a'
-}

--TODO: instance Size pentru data List definit mai sus sau pentru liste simpla Haskell []


class Size a where
  size :: a -> Int
  uniqueSize :: a -> Int

instance Ord a => Size [a] where
  size = foldr (\x acc -> acc + 1) 0
  uniqueSize = size . unique

sort :: Ord a => [a] -> [a]
sort [] = []
sort [x] = [x]
sort (x:list) = g (sort list) where
   g [] = [x]
   g (y:list)
      | x < y = x : y : list
      | otherwise = y : g list

unique :: Ord a => [a] -> [a]
unique [] = []
unique [x] = [x]
unique (x:list) = g (unique list) where
   g [] = [x]
   g (y:list)
      | x == y = y : list
      | x < y = x : y : list
      | otherwise = y : g list