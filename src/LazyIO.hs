{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
module LazyIO where

import System.IO.Unsafe ( unsafeInterleaveIO )

data Style
    = Sequential
    | Applicative
    | Interactive
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
        ; putStrLn (f x y z)
        }
    (Sequential, Lazy) -> do
        { x <- lzGetLine
        ; y <- lzGetLine
        ; z <- lzGetLine
        ; putStrLn (f x y z)
        }
    (Sequential, Mixed) -> do
        { x <- lzGetLine
        ; y <- lzGetLine
        ; z <- lzGetLine
        ; putStrLn (g x y z)
        }
    (Applicative, Strict) -> putStrLn =<< f <$> getLine   <*> getLine   <*> getLine
    (Applicative, Lazy)   -> putStrLn =<< f <$> lzGetLine <*> lzGetLine <*> lzGetLine
    (Applicative, Mixed)  -> putStrLn =<< g <$> lzGetLine <*> lzGetLine <*> lzGetLine
    (Interactive, Mixed)  -> interact h
    _                     -> error "not implemented"
    where
        f a b c    = c ++ a ++ b
        g !a !b !c = f a b c
        h cs = case take 3 (lines cs) of
            [a,b,c] -> unlines [f a b c]

lzGetLine :: IO String
lzGetLine = lazy getLine

lazy :: IO a -> IO a
lazy = unsafeInterleaveIO
