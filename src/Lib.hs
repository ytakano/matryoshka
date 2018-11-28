module Lib
    ( parseFile
    ) where

import qualified Parser as P

text = "letrec fun = \\c send(c, 0) in spawn(fun, ch(0, 1))"

parseFile file = do
    s <- readFile file
    putStrLn $ "input:\n" ++ s
    putStrLn $ "\noutput: " ++ show (P.parse file s)
