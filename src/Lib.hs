module Lib
    ( parseFile
    ) where

import qualified Parser as P

text = "x(3)"

parseFile = do
    print result where result = P.parse "test" text
