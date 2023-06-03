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
