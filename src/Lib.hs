module Lib
    ( parseFile
    ) where

import qualified Parser as P

text = "if (\\x x, y (1), #false) (2) "

parseFile = do
    print result where result = P.parse "test" text
