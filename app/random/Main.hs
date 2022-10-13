module Main where

import System.Random
import System.Random.Shuffle 

type Sized a = ([a], Int)

toSized :: [a] -> Sized a
toSized = (,) <*> length

fromSized :: Sized a -> [a]
fromSized = fst

main :: IO ()
main = undefined

myShuffle :: RandomGen g => g -> [a] -> [a]
myShuffle g = flip (uncurry shuffle') g . toSized 