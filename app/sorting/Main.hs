{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Arrow (second)
import System.Environment
-- import Sort
import Data.Char
import Data.List (delete, mapAccumL,isPrefixOf)

main :: IO ()
main = do
    a:_ <- getArgs
    if "1" `isPrefixOf` a then putStr $ unlines [show $ test1 8]
                          else putStr $ unlines [show $ test2 8]

func :: String -> String
func input = if
    "1" `isPrefixOf` input then unlines [show $ test1 7]
                           else unlines [show $ test2 7]

fibc 0 k = k 0
fibc 1 k = k 1
fibc (n+2) k = fibc (n+1) (fibc n . (k .) . (+))

fib 0 = 0
fib 1 = 1
fib (n+2) = fib (n+1) + fib n

foldrc _   e []     k = k e
foldrc phi e (x:xs) k = foldrc phi e xs $ k . phi x

mapAccumLc :: (a -> b -> (a,c))
          -> a
          -> [b]
          -> (a -> [c] -> s)
          -> s
mapAccumLc phi a0 [] k = k a0 []
mapAccumLc phi a0 (b:bs) k = case phi a0 b of
    (a, c) -> mapAccumLc phi a bs (\ a' cs -> k a' (c:cs))

mapAccumLcc :: (a -> b -> (a -> c -> r) -> r)
            -> a
            -> [b]
            -> (a -> [c] -> r)
            -> r
mapAccumLcc phi a0 []     k = k a0 []
mapAccumLcc phi a0 (b:bs) k
    = phi a0 b (\ a c -> mapAccumLcc phi a bs (\ a' cs -> k a' (c : cs)))

samplePi = map digitToInt $ filter isDigit $ show (pi :: Double)

order, order' :: Ord a => a -> a -> (a, a)
order x y = case compare x y of
    GT -> (y, x)
    _  -> (x, y)

order' x y = case flip compare x y of
    GT -> (y, x)
    _  -> (x, y)

orderc :: Ord a => a -> a -> (a -> a -> r) -> r
orderc !x !y k = case compare x y of
    GT -> k y x
    _  -> k x y

select1, select2, select3, select4 :: Ord a => a -> [a] -> (a, [a])
select1 x xs = (y, ys)
    where
        y  = minimum (x:xs)
        ys = delete y (x:xs)

select2 x xs = mapAccumLcc orderc x xs (,)

select3 x xs = mapAccumL order x xs

select4 x xs = mapAccumL' order x xs

sample :: Int -> [Int]
sample n = reverse $ [1 .. 10^n]

test1, test2, test3 :: Int -> Int
test1 n = fst $ select1 0 $! reverse [1 .. 10^n]
test2 n = fst $ select2 0 $! reverse [1 .. 10^n]
test3 n = fst $ select3 0 $! reverse [1 .. 10^n]
test4 n = fst $ select4 0 $! reverse [1 .. 10^n]

mapAccumL' :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccumL' phi !a0 bbs = case bbs of
    []      -> (a0, [])
    !b : bs -> case phi a0 b of
        (!a, !c)   -> case mapAccumL' phi a bs of
           (!a', cs) -> (a', c:cs)
