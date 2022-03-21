import Data.List

-- 1. Introduction

type Matrix = [[Integer]]

basic = "1 2 3\n4 5 6\n7 8 9"
m = [[1,2,3],[4,5,6],[7,8,9]]
a = [[1,2],[3,4]]
b = [[5,6],[7,8]]


-- 1.1
separator :: Char -> String -> [String]
separator sep = foldr op []
       where
        op c [] | c == sep = []
                | otherwise = [[c]]
        op c (x:xs) | c == sep = "":(x:xs)
                    | otherwise = (c:x):xs

parsem :: String -> Matrix
parsem = (map (map read)).(map (separator ' ')).(separator '\n')


-- 1.2
toString :: Matrix -> String
toString = (bind "\n").(map (bind " ")).(map (map show))
        where bind s = foldr ((++).(++s)) []


-- 1.3
displayMatrix = putStrLn . toString


-- 2. Matrix operations

-- 2.1
vprod :: Integer -> Matrix -> Matrix
vprod v = map (\x -> map (*v) x)


-- 2.2
hjoin :: Matrix -> Matrix -> Matrix
hjoin = zipWith (++)


-- 2.3
vjoin :: Matrix -> Matrix -> Matrix
vjoin = \x -> \y -> x ++ y


-- 2.4
msum :: Matrix -> Matrix -> Matrix
msum = zipWith (\x y -> zipWith (+) x y)


-- 2.5
tr :: Matrix -> Matrix
tr ([]:_) = []
tr m = (map head m):(tr (map tail m))


-- 2.6
value ln col = foldr (+) 0 $ zipWith (+) ln col


mprod :: Matrix -> Matrix -> Matrix
mprod m1 m2 = map (\l -> map (value l) (tr m2)) m1


-- 3. Image operations

type Image = [String]

logo = [l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19]
    where l1 ="        ***** **            ***** **    "
          l2 ="     ******  ****        ******  ****   "
          l3 ="    **   *  *  ***      **   *  *  ***  "
          l4 ="   *    *  *    ***    *    *  *    *** "
          l5 ="       *  *      **        *  *      ** "
          l6 ="      ** **      **       ** **      ** "
          l7 ="      ** **      **       ** **      ** "
          l8 ="    **** **      *      **** **      *  "
          l9 ="   * *** **     *      * *** **     *   "
          l10="      ** *******          ** *******    "
          l11="      ** ******           ** ******     "
          l12="      ** **               ** **         "
          l13="      ** **               ** **         "
          l14="      ** **               ** **         "
          l15=" **   ** **          **   ** **         "
          l16="***   *  *          ***   *  *          "
          l17=" ***    *            ***    *           "
          l18="  ******              ******            "
          l19="    ***                 ***             "

mini = [l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19]
    where l1 ="        ***** **    "
          l2 ="     ******  ****   "
          l3 ="    **   *  *  ***  "
          l4 ="   *    *  *    *** "
          l5 ="       *  *      ** "
          l6 ="      ** **      ** "
          l7 ="      ** **      ** "
          l8 ="    **** **      *  "
          l9 ="   * *** **     *   "
          l10="      ** *******    "
          l11="      ** ******     "
          l12="      ** **         "
          l13="      ** **         "
          l14="      ** **         "
          l15=" **   ** **         "
          l16="***   *  *          "
          l17=" ***    *           "
          l18="  ******            "
          l19="    ***             "


mask = [l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19]
    where l1 ="                       *****************"
          l2 ="                       *****************"
          l3 ="                       *****************"
          l4 ="                       *****************"
          l5 ="                       *****************"
          l6 ="                       *****************"
          l7 ="                       *****************"
          l8 ="                       *****************"
          l9 ="                       *****************"
          l10="                       *****************"
          l11="                       *****************"
          l12="                       *****************"
          l13="                       *****************"
          l14="                       *****************"
          l15="                       *****************"
          l16="                       *****************"
          l17="                       *****************"
          l18="                       *****************"
          l19="                       *****************"


-- 3.1
toStringImg :: Image -> String
toStringImg x = foldr (++) [] $ map (++ "\n") x 

displayImg = putStrLn . toStringImg


-- 3.2
flipH :: Image -> Image 
flipH = map reverse

displayH = displayImg . flipH


-- 3.3
flipV :: Image -> Image
flipV = reverse

displayV = displayImg . flipV 


trImg :: Image -> Image
trImg ([]:_) = []
trImg m = (map head m):(trImg (map tail m))


-- 3.4
rotate90r :: Image -> Image
rotate90r = flipH . trImg


-- 3.5
rotate90l :: Image -> Image
rotate90l = flipV . trImg


-- 3.6
invert :: Image -> Image
invert i = map op i
         where op [] = []
               op (x:xs) | x == ' ' = '*' : op xs
                         | otherwise = ' ' : op xs


-- 3.7
maskKeep :: Image -> Image -> Image
maskKeep [] _ = []
maskKeep (x:xs) (y:ys) = (checkK x y ) : (maskKeep xs ys)

checkK :: String -> String -> String
checkK _ [] = []
checkK (x:xs) (y:ys) | x == y = x : checkK xs ys
                    | otherwise = ' ' : checkK xs ys

-- 3.8
maskDiscard :: Image -> Image -> Image
maskDiscard [] _ = []
maskDiscard (x:xs) (y:ys) = (checkD x y ) : (maskDiscard xs ys)

checkD :: String -> String -> String
checkD _ [] = []
checkD (x:xs) (y:ys) | x /= y = x : checkD xs ys
                     | otherwise = ' ' : checkD xs ys


-- 3.9
union' :: Image -> Image -> Image
union' [] _ = []
union' (x:xs) (y:ys) = (checkU x y ) : (union' xs ys)

checkU _ [] = []
checkU (x:xs) (y:ys) | x /= ' ' = x : checkU xs ys
                     | y /= ' ' = y : checkU xs ys
                     | otherwise = ' ' : checkU xs ys


-- 3.10
transformationSequence :: [Image -> Image] -> Image -> Image
transformationSequence _ [] = []
transformationSequence [] img = img
transformationSequence (x:xs) img = transformationSequence xs (x img) 


seq1 = transformationSequence [invert, union' mask, rotate90r]


-- 4. More matrices

-- 4.1
zAbove :: Matrix -> Matrix
zAbove = undefined