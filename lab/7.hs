--1 
data Tree a = Null | Node a [Tree a] deriving Show

--2
instance Functor Tree where
    fmap _ Null = Null
    fmap f (Node x t) = Node (f x) (fmap (fmap f) t)

--3
instance Foldable Tree where
    foldr _ acc Null = acc
    foldr f acc (Node x t) = f x (foldr (\t' acc -> foldr f acc t') acc t)

--4
class Zippable f where
    zipp :: (a -> b -> c) -> f a -> f b -> f c

--5
instance Zippable [] where
    zipp _ _ [] = []
    zipp _ [] _ = []
    zipp f (x:xs) (y:ys) = (f x y) : (zipp f xs ys)

--6
instance Zippable Maybe where
    zipp _ _ Nothing = Nothing
    zipp _ Nothing _ = Nothing
    zipp f (Just x) (Just y) = Just (f x y)

--7
instance Zippable (Either a) where
    zipp _ _ (Left x) = (Left x)
    zipp _ (Left x) _ = (Left x)
    zipp f (Right x) (Right y) = Right (f x y)

--8
data BTree a = BNull | BNode a (BTree a) (BTree a) deriving Show

instance Zippable BTree where
    zipp _ _ BNull = BNull
    zipp _ BNull _ = BNull
    zipp f (BNode e l r) (BNode x y z) = BNode (f e x) (zipp f l y) (zipp f r z)

--9
instance Zippable Tree where
    zipp _ _ Null = Null
    zipp _ Null _ = Null
    zipp f (Node x t) (Node y z) = Node (f x y) (zipp (zipp f) t z) 

--10
-- instance Zippable ((,) a) where
    -- zipp x y = 

instance Zippable ((->) a) where
    zipp f x y = \z -> f (x z) (y z) 