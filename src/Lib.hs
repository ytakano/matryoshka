module Lib
    ( parseFile
    ) where

import qualified Matryoshka as M

text = "#1234"

parseFile = do
    print result where result = M.parse "test" text
