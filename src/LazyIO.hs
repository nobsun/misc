{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
module LazyIO where

import System.IO.Unsafe ( unsafeInterleaveIO )

data Style
    = Sequential
    | PointWise
    | PointFree
    deriving (Eq, Show, Read)

data IOStyle
    = Strict
    | Lazy
    | Mixed
    deriving (Eq, Show, Read)

data Experiment
    = SkewAppend
    deriving (Eq, Show, Read)

experiment :: (Style, IOStyle) -> Experiment -> IO ()
experiment opts = \ case
    SkewAppend -> skewappend opts

skewappend :: (Style, IOStyle) -> IO ()
skewappend = \ case 
    (Sequential, Strict) -> do
        { x <- getLine
        ; y <- getLine
        ; z <- getLine
        ; putStrLn (z ++ x ++ y)
        }
    (Sequential, Lazy) -> do
        { x <- lzGetLine
        ; y <- lzGetLine
        ; z <- lzGetLine
        ; putStrLn (z ++ x ++ y)
        }
    _           -> error "not yet"

lzGetLine :: IO String
lzGetLine = lazy getLine

lazy :: IO a -> IO a
lazy = unsafeInterleaveIO
