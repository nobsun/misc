module Trapezoid where

{-
(a + b) * b / 2 = 10^6 * a + b

ab + b^2 =  2 * 10^6 a + 2 * b
b^2 + (a-2) b = 2000000 a
b * (a+b-2)   = 2^7 * 5^6 * a
-}

trapezoid1, trapezoid2, trapezoid4, trapezoid5 :: [(Int, Int)]
trapezoid1 = [ (a,b) | a <- [100000 .. 999998], b <- map (a *) [1..9]
                    , (a + b) * b `div` 2 == 10^6 * a + b ]

trapezoid2 = [ (a,b) | a <- [100000, 100002 .. 999998]
                     , b <- takeWhile (1000000 >) $ map (div a 2 *) [1..19]
                     , (a + b) * b `div` 2 == 10^6 * a + b ]

trapezoid4 = [ (a,b) | a <- [100000, 100004 .. 999998]
                     , b <- takeWhile (1000000 >) $ map (div a 4 *) [1..39]
                     , (a + b) * b `div` 2 == 10^6 * a + b ]

trapezoid5 = [ (a,b) | a <- [100000, 100005 .. 999998]
                     , b <- takeWhile (1000000 >) $ map (div a 5 *) [1..49]
                     , (a + b) * b `div` 2 == 10^6 * a + b ]
trapezoid :: Int -> [(Int,Int)]
trapezoid n = [ (a,b) | a <- takeWhile (1000000>) $ dropWhile (100000 >) $ map (n*) [1..]
                      , b <- takeWhile (1000000 >) $ map (div a n *) [1..]
                      , (a + b) * b `div` 2 == 10^6 * a + b ]

factors :: [Int]
factors = 1 : merge (map (2*) factors)
                    (map (5*) factors)


merge :: Ord a => [a] -> [a] -> [a]
merge xxs@(x:xs) yys@(y:ys) = case compare x y of
    EQ -> x : merge xs ys
    LT -> x : merge xs yys
    GT -> y : merge xxs ys

merge' :: Ord a => [a] -> [a] -> [a]
merge' [] yys = yys
merge' xxs [] = xxs
merge' xxs@(x:xs) yys@(y:ys) = case compare x y of
    EQ -> x : merge' xs ys
    LT -> x : merge' xs yys
    GT -> y : merge' xxs ys

xmerge [] ys = ys
xmerge (x:xs) ys = x : merge' xs ys

merges :: Ord a => [[a]] -> [a]
merges = foldr1 xmerge