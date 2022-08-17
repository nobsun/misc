module Main where

main :: IO ()
main = putStr (unlines (fizzbuzz 1 15))

fizzbuzz :: Int -> Int -> [String]
fizzbuzz from to = take to (drop from fzbz)

fzbz :: [String]
fzbz = zipWith3 f fizz buzz [0 :: Int ..]
    where f :: String -> String -> Int -> String
          f s t n = (s ++ t) <+ show n
 
fizz :: [String]
fizz = cycle (take 3 ("Fizz" : repeat ""))

buzz :: [String]
buzz = cycle (take 5 ("Buzz" : repeat ""))

(<+) :: [a] -> [a] -> [a]
[] <+ ys = ys
xs <+ _  = xs
