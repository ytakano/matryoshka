{-# LANGUAGE FlexibleContexts #-}

module Parser
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

parseExpr = do
    e1 <- (P.try parseIf) <|>
          parseValue
    e2 <- (P.try $ parseApply e1) <|> return Nothing
    return $ case e2 of
        Nothing -> e1
        Just e  -> e

parseValue = do
    val <- (P.try parseNum)    <|>
           (P.try parseAtom)   <|>
           (P.try parseLambda) <|>
           parseVar
    return $ Val val

parseNum = do
    h <- P.oneOf ['1'..'9']
    t <- P.many P.digit
    return . Num . read $ h:t

parseAtom = do
    h <- P.char '#'
    t0 <- P.alphaNum
    t1 <- P.many P.alphaNum
    return . Atom $ h:t0:t1

parseLambda = do
    _ <- P.char '\\'
    h <- (P.try $ P.oneOf ['a'..'z']) <|> P.oneOf ['A'..'Z']
    t <- P.many P.alphaNum
    _ <- P.space
    _ <- P.spaces
    expr <- parseExpr
    return $ Lambda (h:t) expr

parseVar = do
    h <- (P.try $ P.oneOf ['a'..'z']) <|> P.oneOf ['A'..'Z']
    t <- P.many P.alphaNum
    return $ Var (h:t)

parseIf = do
    _ <- P.string "if"
    _ <- P.spaces
    _ <- P.char '('
    _ <- P.spaces

    e1 <- parseExpr
    _ <- P.spaces
    _ <- P.char ','
    _ <- P.spaces

    e2 <- parseExpr
    _ <- P.spaces
    _ <- P.char ','
    _ <- P.spaces

    e3 <- parseExpr
    _ <- P.spaces
    _ <- P.char ')'

    return $ If e1 e2 e3

parseApply expr = do
    _ <- P.spaces
    _ <- P.char '('
    arg <- parseExpr
    _ <- P.spaces
    _ <- P.char ')'
    apply <- (P.try $ parseApply (Apply expr arg)) <|> return Nothing
    return $ case apply of
        Nothing -> Just $ Apply expr arg
        Just a  -> Just $ a

parse :: String -> String -> Either P.ParseError Expr
parse file text = P.parse parseExpr file text
