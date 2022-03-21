import Data.List
import Data.Maybe
import Data.Char

e1 :: Int -> Int
e1 x = op x 1
   where
      op x ind | x - ind /= 0 = (op (x-ind) (ind + 1))
               | x - ind == 0 = ind
               | otherwise = -1 