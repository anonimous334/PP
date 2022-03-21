import Data.List
import Data.Maybe
import Data.Char


--1

line :: [Char] -> Char -> Bool
line [] _ = True
line (x:xs) p | x == p = line xs p 
              | otherwise = False 
allLines :: [String] -> Char -> Bool
allLines [[]] _ = True
allLines (x:xs) p | line x p == True = allLines xs p
                  | otherwise = False


col :: [String] -> Char -> Bool

s1 :: [Char] -> Char -> Bool
s1 mat p = undefined
--2 

--3

--4