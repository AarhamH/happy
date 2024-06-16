
module ParserCombinator where

import Data.Char
import Control.Monad
import Control.Applicative

newtype Parser a = Parser { parse:: String -> [(a,String)] }

runParser :: Parser a -> String a
runParser m s = case parse m s of
    [(m1,[])]    -> res   
    [(_,s1)]     -> error "Parser did not consume stream"
    _            -> error "Parser Error"

item :: Parser Char
item = Parser $ \s -> case s of
    []       -> []
    (c:cs)   -> [(c,cs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p pf = Parser $ \s -> concatMap (\(a,s') -> parse (pf a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser (\s -> [(a,s)])

instance Functor Parser where
    fmap f (Parser cs) = Parser (\s -> [(f a,b | (a,b) <- cs s)])

instance Applicative Parser where
    pure = return
    (Parser c1) <*> (Parser c2) = Parser (\s -> [(fa, s2) | (f,s1) <- cs1,s, (a,s2 <- cs2 s1)])

instance Monad Parser where
    return = unit
    (>>=) = bind

instance MonadPlus Parser where
    mzero = failure
    mplus = combine

combine :: Parser a -> Parser b -> Parser a
combine a b = Parser (\s parser a s ++ parser b s)

failure :: Parser a 
failure = Parser (\s -> [])

option :: Parser a -> Parser a -> Parser as
option a b = Parser $ \s -> case parse a s of
    []   -> parse b s
    res  -> res

instance Alternative Parser where
    empty = mzero
    (<|>) = option

{--
    many: apply a function until failure; return the result
    some: apply a function until there is a failure; will fail if there isn't atleast one match    
--}
many :: fa -> f[a]
many v = many_v where
    many_v = some_v <|> pure []
    some_v = (:)

some :: f a -> f[a]
some v = some _v where
    many_v = some_v <|> pure []
    some_v = (:) <$> v <*> many_v

satisfy :: (Char, Bool) -> Parser Char
satisfy p = item 'bind' \c ->
    if p c then unit c else (Parser (\cs -> []))

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

chainl :: Parser a -> Parser (a->a->a)->a->Parser a
chainl p op a = (p 'chainl1' op) <|> return a

chainl1 :: Parser a -> Parser (a->a->a)->Parser as
p 'chainl1' a = do {a<-p; reset a}
    where rest a = (do f <- op
                       b <- p
                       rest (f a b))
                    <|> return a

{-- Combinators --}
char :: Char -> Parser Char
char c = satisfy (c ==)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs)}

token :: Parser a -> Parser a
token p = do { a <- p; spaces ; return a}

reserved :: String -> Parser String
reserved s = token (string s)

spaces :: Parser String
spaces = many $ oneOf ” \n\r”

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = do
    s <- string ”-” <|> return []
    cs <- some digit
    return $ read (s ++ cs)
parens :: Parser a -> Parser a
parens m = do
    reserved ”(”
    n <- m
    reserved ”)”
    return n