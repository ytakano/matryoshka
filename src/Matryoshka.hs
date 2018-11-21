{-# LANGUAGE FlexibleContexts #-}

module Matryoshka
    ( Value(..),
      Expr(..),
      parse
    ) where

import           Control.Monad
import           Control.Monad.Identity (Identity)
import qualified Text.Parsec            as P

data Value = Atom String
           | Num Int
           | Var String
           | PFun String
           | Lambda String Expr deriving (Show)

data Expr = Val Value
          | Apply Expr Expr
          | PrimtiveFun String [Expr]
          | If Expr Expr Expr
          | Spawn Expr [Expr]
          | Clone Expr [Expr]
          | Ch Int Int
          | Send Expr Expr
          | Recv Expr deriving (Show)

parserTop = do
    return $ Val (Num 10)

parseHelper rule text = P.parse rule "(stdin)" text

parse :: String -> Either P.ParseError Expr
parse expr = parseHelper parserTop expr
