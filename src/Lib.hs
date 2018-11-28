module Lib
    ( parseFile
    ) where

import qualified Parser as P

text = "letrec fun = \\c send(c, 0) in spawn(fun, ch(0, 1))"

parseFile = do
    print result where result = P.parse "test" text
