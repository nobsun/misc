module Main where

import LazyIO

main :: IO ()
main = experiment (Sequential, Strict) SkewAppend 
    >> experiment (Sequential, Lazy) SkewAppend
    
