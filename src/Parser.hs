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
--import Debug.Trace

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
          | Recv Expr
          | Let String Expr Expr
          | Letrec String Expr Expr deriving (Show)

parseExpr = do
    _ <- P.spaces
    h <- P.oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['\\'] ++ ['#'] ++ ['0'..'9'] ++ ['(']

    e1 <- case h of
        '\\' -> do
            e <- parseLambda
            return $ Val e
        '#' -> do
            e <- parseAtom
            return $ Val e
        '(' -> do
            _ <- P.spaces
            e <- parseExpr
            _ <- P.spaces
            _ <- P.char ')'
            return e
        otherwise -> parseNumFuncVar h

    e2 <- parseApply e1
    return $ case e2 of
        Nothing -> e1
        Just e  -> e

parseNumFuncVar h
    | '0' <= h && h <= '9' = do
        e <- case h of
            '0' -> do
                return $ Num 0
            otherwise -> parseNum h
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
    | s == "send" = parseSend
    | s == "recv" = parseRecv
    | s == "spawn" = parseSpawn
    | s == "let" = parseLet
    | s == "letrec" = do
        Let v e1 e2 <- parseLet
        return $ Letrec v e1 e2
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

        arg <- parseExpr

        _ <- P.spaces
        _ <- P.char ')'

        apply <- parseApply $ Apply expr arg
        return $ case apply of
            Nothing -> Just $ Apply expr arg
            Just a  -> Just $ a

parseInt = do
    h <- P.digit
    n <- case h of
        '0' -> do
            return "0"
        otherowse -> do
            t <- P.many P.digit
            return $ h:t
    return $ read n

parseCh = do
    _ <- P.spaces
    _ <- P.char '('
    _ <- P.spaces

    n1 <- parseInt

    _ <- P.spaces
    _ <- P.char ','
    _ <- P.spaces

    n2 <- parseInt

    _ <- P.spaces
    _ <- P.char ')'
    _ <- P.spaces

    return $ Ch n1 n2

parseSend = do
    _ <- P.spaces
    _ <- P.char '('
    _ <- P.spaces

    e1 <- parseExpr
    _ <- P.spaces
    _ <- P.char ','
    _ <- P.spaces

    e2 <- parseExpr
    _ <- P.spaces
    _ <- P.char ')'

    return $ Send e1 e2

parseRecv = do
    _ <- P.spaces
    _ <- P.char '('
    _ <- P.spaces

    e <- parseExpr

    _ <- P.spaces
    _ <- P.char ')'

    return $ Recv e

parseClone = do
    a:args <- parseArgs
    return $ Clone a args

parseSpawn = do
    a:args <- parseArgs
    return $ Spawn a args

parseArgs = do
    _ <- P.spaces
    _ <- P.char '('

    args <- parseArgs2 []

    _ <- P.spaces
    _ <- P.char ')'

    return $ reverse args

parseArgs2 args = do
    _ <- P.spaces
    e <- parseExpr
    _ <- P.spaces
    c <- (P.try $ P.char ',') <|> return ' '

    result <- case c of
        ',' -> do
            e2 <- parseArgs2 $ e:args
            return e2
        otherwise -> do
            return $ e:args

    return result

parseLet = do
    _ <- P.space
    _ <- P.spaces

    h <- P.oneOf $ ['a'..'z'] ++ ['A'..'Z']
    t <- P.many P.alphaNum

    _ <- P.spaces
    _ <- P.char '='
    _ <- P.spaces

    e1 <- parseExpr

    _ <- P.spaces
    _ <- P.string "in"
    _ <- P.space

    e2 <- parseExpr

    return $ Let (h:t) e1 e2

parse :: String -> String -> Either P.ParseError Expr
parse file text = P.parse parseExpr file text
