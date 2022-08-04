module LazyList where

import Control.Concurrent
import Data.Char
import System.Environment
import System.IO
import System.IO.Unsafe

main :: IO ()
main = lazyGen

lazyGen :: IO ()
lazyGen = do 
    { as <- getArgs
    ; case as of
        n : δ : ss 
            | all isDigit (n ++ δ)
                -> putStr . unlines . take (read n) =<< delayList (10^3 * read δ) (cycle ss)
        _       -> putStrLn "Usage: lzgen <length> <interval ms> <elememt> [<element>]*"
    }

delayList :: Int -> [a] -> IO [a]
delayList _ [] = unsafeInterleaveIO $ return []
delayList i (x:xs) 
   = unsafeInterleaveIO $ threadDelay i >> delayList i xs >>= return . (x:) 