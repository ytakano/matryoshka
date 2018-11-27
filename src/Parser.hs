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
import Debug.Trace

data Value = Atom String
           | Num Int
           | Var String
           | Lambda String Expr deriving (Show)

data Expr = Val Value
          | Apply Expr Expr
          | If Expr Expr Expr
          | Spawn Expr [Expr]
          | Clone Expr [Expr]
          | Ch Int Int
          | Send Expr Expr
          | Recv Expr deriving (Show)

parseExpr = do
    _ <- P.spaces
    h <- P.oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['\\'] ++ ['#'] ++ ['1'..'9']

    e1 <- case h of
        '\\' -> do
            e <- parseLambda
            return $ Val e
        '#' -> do
            e <- parseAtom
            return $ Val e
        otherwise -> parseNumFuncVar h

    e2 <- parseApply e1
    return $ case e2 of
        Nothing -> e1
        Just e -> e

parseNumFuncVar h
    | '1' <= h && h <= '9' = do
        e <- parseNum h
        return $ Val e
    | otherwise = parseFuncVar h

parseFuncVar h = do
    t <- P.many P.alphaNum
    parseFuncVar2 $ h:t

parseFuncVar2 s = do
    e <- parseFuncVar3 s
    return e

parseFuncVar3 s
    | s == "if" = parseIf
    | s == "ch" = parseCh
    | otherwise = do
        return . Val $ Var s

parseNum h = do
    t <- P.many P.digit
    return . Num . read $ h:t

parseAtom = do
    t0 <- P.alphaNum
    t1 <- P.many P.alphaNum
    return . Atom $ '#':t0:t1

parseLambda = do
    h <- P.oneOf $ ['a'..'z'] ++ ['A'..'Z']
    t <- P.many P.alphaNum
    _ <- P.space
    _ <- P.spaces
    expr <- parseExpr
    return $ Lambda (h:t) expr

parseIf = do
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
    p <- (P.try $ P.char '(') <|> return '-'
    e <- parseApply2 p expr
    return e

parseApply2 p expr
    | p /= '(' = do
        return Nothing
    | otherwise = do
        _ <- P.spaces

        arg <- (trace $ "expr: " ++ show expr) $ parseExpr

        _ <- P.spaces
        _ <- P.char ')'

        apply <- parseApply $ Apply expr arg
        return $ (trace $ show apply ) $ case apply of
            Nothing -> Just $ Apply expr arg
            Just a  -> Just $ a

parseCh = do
    _ <- P.spaces
    _ <- P.char '('
    _ <- P.spaces

    h1 <- P.oneOf ['1'..'9']
    t1 <- P.many P.digit

    _ <- P.spaces
    _ <- P.char ','
    _ <- P.spaces

    h2 <- P.oneOf ['1'..'9']
    t2 <- P.many P.digit

    _ <- P.spaces
    _ <- P.char ')'
    _ <- P.spaces

    return $ Ch (read $ h1:t1) (read $ h2:t2)

parse :: String -> String -> Either P.ParseError Expr
parse file text = P.parse parseExpr file text
