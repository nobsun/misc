module DropEvery where

import Data.List

dropEvery, dropEvery' :: Int -> [a] -> [a]
dropEvery _ [] = []
dropEvery n xs = take (n-1) xs ++ dropEvery n (drop n xs)

dropEvery' n = concat . unfoldr f
    where
        f [] = Nothing
        f xs = Just (take (n-1) xs, drop n xs)