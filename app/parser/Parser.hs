{-# OPTIONS_GHC -Wno-unused-matches #-}
module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Values
import Control.Monad
import Control.Monad.Except

symbol :: Parser Char 
symbol = oneOf "!#$%&|*+-/:<=>?@^_~a"

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser Values
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom

parseList :: Parser Values
parseList = liftM List $ sepBy parseExpr spaces

parseImproperList :: Parser Values
parseImproperList = do
    lhead <- endBy parseExpr spaces
    ltail <- char '.' >> spaces >> parseExpr
    return $ ImproperList lhead ltail

parseNumber :: Parser Values
parseNumber = liftM (Number . read) $ many1 digit

parseQuoted :: Parser Values
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser Values
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do _ <- char '('
               x <- try parseList <|> parseImproperList
               _ <- char ')'
               return x

parseString :: Parser Values
parseString = do
    _ <- char '"'
    x <- many (noneOf "\"")
    _ <- char '"'
    return $ String x

readExpr :: String -> ThrowsError Values
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val
