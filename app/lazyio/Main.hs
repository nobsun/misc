{-# LANGUAGE BangPatterns #-}
module Main where

import LazyIO
import System.IO.Unsafe

{- --
main :: IO ()
main = experiment (Sequential, Strict) SkewAppend 
    >> experiment (Sequential, Lazy) SkewAppend
    >> experiment (Sequential, Mixed) SkewAppend
    >> experiment (Applicative, Strict) SkewAppend
    >> experiment (Applicative, Lazy) SkewAppend
    >> experiment (Applicative, Mixed) SkewAppend
    >> experiment (Interactive, Mixed) SkewAppend
-- -}

{- --
main :: IO ()
main = do
    { a <- getLine
    ; b <- getLine
    ; c <- getLine
    ; putStr (unlines [c, a, b])
    }
-- -}

{- -}
main :: IO ()
main = do
    a <- getLine'
    b <- getLine'
    c <- getLine'
    putStr (unlines [c, b])
-- -}
{- -}

getLine' :: IO String
getLine' = unsafeInterleaveIO getLine
-- -}

{- --
main :: IO ()
main = do
    a <- getLine'
    b <- getLine'
    c <- getLine'
    putStr (unlines (f a b c))
    where
        f !a !b !c = [c, a, b]
-- -}