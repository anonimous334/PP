f1_4 :: [Integer] -> [Integer]
f1_4 [] = []
f1_4 [x] = [x]
f1_4 (x:xs) = x : (f1_4(rm x xs))
    where 
        rm :: Integer -> [Integer] -> [Integer]
        rm x [] = []
        rm x (y:ys) | x /= y = y : rm x ys
                    | otherwise = rm x ys
                    
                    
addTrues :: [Bool] -> Integer
addTrues [] = 0
addTrues (x:xs) | x == True = 1 + addTrues xs


filtM :: [[Char]] -> [[Char]]
filtM [] = []
filtM (x:xs) | head(x) == 'M' = x : filtM xs
             | otherwise = filtM xs


f3_4 :: [Char] -> [([Char], [[Char]])] -> [[Char]]
f3_4 gr (x:xs) | fst(x) == gr = filtM (snd(x))
               | otherwise = f3_4 gr xs
