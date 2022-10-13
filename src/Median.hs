{-# LANGUAGE LambdaCase #-}
module Median where

import Data.Bool
import Data.List hiding (insert)
import Data.Maybe

slices :: Int -> [a] -> [([a],Int)]
slices n = unfoldr psi
    where
        psi [] = Nothing
        psi xs = case splitAt n xs of
            (ys,[]) -> Just ((ys,length ys),[])
            (ys,zs) -> Just ((ys,n),zs)

dividg :: Ord a => ([a],Int) -> ([a],a,[a])
dividg (xs,n) = case splitAt h $ isort xs of
    (ys,z:zs) -> (ys,z,zs)
    where
        h = case n `divMod` 2 of
            (q,0) -> q - 1
            (q,_) -> q

para :: (a -> ([a],b) -> b) -> b -> [a] -> b
para phi e = \ case
    []   -> e
    x:xs -> phi x (xs, para phi e xs)

isort :: Ord a => [a] -> [a]
isort = foldr insert []

insert :: Ord a => a -> [a] -> [a]
insert x = para phi [x]
    where
        phi y (ys,zs)
            | x > y     = y : zs 
            | otherwise = x : y : ys

sample :: [Int]
sample = [3,1,4,1,5,9,2,6,5,3,5,8,9,7,9,3]
