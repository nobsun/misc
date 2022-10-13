{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Sort where

import Data.Functor.Foldable
import Data.List (mapAccumR, minimum)
import Data.Maybe

isort :: Ord a => [a] -> [a]
isort = cata insert

insert :: Ord a => ListF a [a] -> [a]
insert = \ case
    Nil -> []
    Cons x xs -> apo psi xs
        where
            psi = \ case
                []               -> Cons x (Left [])
                y:ys | x > y     -> Cons y (Right ys)
                     | otherwise -> Cons x (Left (y:ys))

ssort :: Ord a => [a] -> [a]
ssort = ana select

select :: Ord a => [a] -> ListF a [a]
select = \ case
    []   -> Nil
    x:xs -> list Nil Cons (para phi xs)
        where
            phi = \ case
                Nil -> [x]
                Cons y (ys,zs)
                    | x > y     -> y : zs
                    | otherwise -> x : uncurry (:) (mapAccumR ordering y ys)

ordering :: Ord a => a -> a -> (a, a)
ordering x y = case compare x y of
    GT -> (y, x)
    _  -> (x, y)

sample :: [Int]
sample = [3,1,4,1,5,9,2,6,5,3,5,8,9,7,9,3]

select1, select2 :: Ord a => a -> [a] -> (a, [a])
select1 x xs = (y, ys)
    where
        y  = minimum (x:xs)
        ys = delete y (x:xs)

select2 x xs = mapAccumR ordering x xs

list :: b -> (a -> [a] -> b) -> [a] -> b
list e f = \ case
    x:xs -> f x xs
    _    -> e

-- mapAccumL :: (acc -> a -> (acc, b)) -> acc -> [a] -> (acc, [b])
-- mapAccumL f a = \ case
--     x:xs -> case f a x of
--         (a', b) -> case mapAccumL f a' xs of
--             (a'', bs) -> (a'', b:bs)
--     [] -> (a,[])

-- minimum' :: Ord a => [a] -> a
-- minimum' = foldr1 min

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (y:ys)
    | x /= y    = y : delete x ys
    | otherwise = ys

test :: Int -> Int -> (Int, Int)
test 1 n = second length $ select1 (10^n `div` 2) [1 .. 10^n]
test 2 n = second length $ select2 (10^n `div` 2) [1 .. 10^n]

second :: (b -> c) -> (a, b) -> (a, c)
second g (x, y) = (x, g y)


selectionSort :: (Ord a) => [a] -> [a]  
selectionSort [] = []  
selectionSort xs =   
    let maxi = maximum xs  
        mini = minimum xs  
    in  selectionSort [y | y <- xs, y /= maxi, y /= mini] ++  
        [mini] ++ [maxi]

selectSort :: (Ord a) => [a] -> [a]
selectSort [] = []
selectSort xs = let
    getMin :: (Ord a) => [a] -> a
    getMin [x] = x
    getMin (x:xs) = min x (getMin xs)
  in
    getMin xs : selectSort (filter (/= getMin xs) xs)
