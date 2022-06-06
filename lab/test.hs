f1 :: String -> String -> String -> String
f1 [] _ _ = []
f1 l t1 t2
    | (take (length t1) l) == t1 = t2 ++ (f1 (drop (length t1) l) t1 t2)
    | otherwise = (head l) : (f1 (tail l) t1 t2)
    
    (x:y:xs) = (x:xs)++[y]
    		     ++(y:[])
    		    
    		    
    foldr (*) 1 [2,3,4,5]
    5
    20
    60
    120
    foldl (*) 1 [2,3,4,5]
    splits c text
    	| []		    
