module Lib
    ( parseFile
    ) where

import qualified Parser as P

text = "(\\x x(3))(y)(z)"

parseFile = do
    print result where result = P.parse "test" text
