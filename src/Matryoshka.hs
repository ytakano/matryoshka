{-# LANGUAGE FlexibleContexts #-}

module Matryoshka
    ( Value(..),
      Expr(..),
      parse
    ) where

import           Control.Monad
import           Control.Monad.Identity (Identity)
import           Text.Parsec            ((<|>))
import qualified Text.Parsec            as P

data Value = Atom String
           | Num Int
           | Var String
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
    val <- parseValue
    return $ val

parseValue = do
    val <- (P.try parseNum) <|> parseAtom
    return val

parseNum = do
    h <- P.oneOf ['1'..'9']
    t <- P.many P.digit
    return . Val . Num . read $ h:t

parseAtom = do
    h <- P.char '#'
    t0 <- P.alphaNum
    t1 <- P.many P.alphaNum
    return . Val . Atom $ h:t0:t1

parse :: String -> String -> Either P.ParseError Expr
parse file text = P.parse parserTop file text
