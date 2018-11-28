module Main where

import           Lib
import           System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    parseFile $ args !! 0
    return ()
