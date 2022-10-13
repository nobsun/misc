{-# LANGUAGE LambdaCase #-}
module VM where

import Data.Bool
import Data.Char
import Data.List.Extra
import Data.Maybe
import System.Environment

vm :: IO ()
vm = interact . drive . runVM
   =<< list (return sample) ((readFile .) . const)
   =<< getArgs

sample :: String
sample = ""

drive :: ([String] -> [String]) -> (String -> String)
drive f = unlines . f . lines

runVM :: String -> [String] -> [String]
runVM src = mapMaybe output . exec . initMachine src

data VM
    = VM
    { inputs :: [String]
    , output :: Maybe String
    , istate :: InnerState
    }

data InnerState 
    = InnerState

initMachine :: String -> [String] -> VM
initMachine src ins
    = VM
    { inputs = ins
    , output = maybeOutput "" state
    , istate = state
    }
    where
        state = InnerState

compile :: String -> InnerState
compile src = InnerState

exec :: VM -> [VM]
exec m
    = m : rests
    where
        rests
            | final m   = []
            | otherwise = exec (step m)

final :: VM -> Bool
final = null . inputs

step :: VM -> VM
step m = case m of
    VM { inputs = i:is
       , istate = oldState
       } -> m { inputs = is
              , output = newOutput
              , istate = newState 
              }
            where
                newState  = nextState   i oldState
                newOutput = maybeOutput i newState

nextState :: String -> InnerState -> InnerState
nextState inp old = old

maybeOutput :: String -> InnerState -> Maybe String
maybeOutput inp new = case inp of
    "" -> Nothing
    _  -> Just $ map toUpper inp
