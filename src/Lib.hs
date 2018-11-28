module Lib
    ( parseFile
    ) where

import qualified Parser as P

text = "letrec fun = \\c send(c, 0) in spawn(fun, ch(0, 1))"

parseFile = do
    putStrLn $ "input: " ++ text
    putStrLn $ "output: " ++ show (P.parse "test" text)
