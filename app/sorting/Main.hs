module Main where

import Data.List
import Data.Ord

main :: IO ()
main = interact proc

proc :: String -> String
proc str = case map (map readInt . words) (lines str) of
    _ : [as] -> bubbleSorting as

---

bubbleSorting :: [Int] -> String
bubbleSorting = showResult . beval

beval :: [Int] -> [[Int]]
beval as = case bstep (0, as) of
    (0, bs) -> []
    (c, bs) -> bs : beval bs

bstep :: (Int, [Int]) -> (Int, [Int])
bstep s@(c, [x]) = s
bstep (c, x:yys@(y:ys))
    | x > y     = case bstep (succ c, x:ys) of
        (c', zs) -> (c', y:zs)
    | otherwise = case bstep (c, yys) of
        (c', zs) -> (c', x:zs)

---

selectionSorting :: [Int] -> String
selectionSorting = showResult . map (uncurry ($)) . seval . (,) id

seval :: ([Int] -> [Int], [Int]) -> [([Int] -> [Int], [Int])]
seval cs = case cs of 
    (c, []) -> []
    (c, xs) -> cs : seval (sstep cs)


sstep :: ([Int] -> [Int], [Int]) -> ([Int] -> [Int], [Int])
sstep (c, xs) = case select xs of
    (y, ys) -> (c . (y :), ys)

select :: Ord a => [a] -> (a, [a])
select (x : []) = (x, [])
select (x : xs) = case minimumBy (comparing snd) (zip [0..] xs) of
    (i, y) 
        | x <= y    -> (x, xs)
        | otherwise -> (y, swap i x xs)
    where
        swap j z ys = case splitAt j ys of
            (zs, _:ws) -> zs ++ z : ws
{- --
select :: Ord a => [a] -> (a, [a])
select (x : []) = (x, [])
select (x : xs)
    | x > y     = (y, x:ys)
    | otherwise = (x, y:ys)
    where
        (y, ys) = select xs
-- -}

---

showResult :: [[Int]] -> String
showResult = unlines . map (unwords . map show)

readInt :: String -> Int
readInt = read

