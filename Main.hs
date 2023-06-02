-- | build a Turing complete calculator based on functional Parsing

module Main where

import System.IO
import Control.Exception
import Data.Functor
import Control.Monad
import Exp
import Parser

-- | parse the string, evaluate the expression, and return the expression as string
calc :: String -> String
calc = (++ "\n") . show . eval . read

-- | Prompt for input
prompt :: String -> IO String
prompt msg = putStr msg >> hFlush stdout >> getLine

-- | catch an error
catchError :: IO a -> (ErrorCall -> IO a) -> IO a
catchError = catch 

-- | put everything together...
main :: IO ()
main = forever $ 
    catchError (prompt "> " <&> calc >>= putStr) $ \_ ->
        putStrLn "parsing error occured, please enter again"


{- for recursion: 
let fib = (\x -> if x == 0 then 1 else 
        if x == 1 then 1 else fib (x - 2) +  fib (x - 1)) in fib 6
-}