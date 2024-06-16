
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

instance Alternative Parser where
    empty = mzero
    (<|>) = option

combine :: Parser a -> Parser b -> Parser a
combine a b = Parser (\s parser a s ++ parser b s)

failure :: Parser a 
failure = Parser (\s -> [])

option :: Parser a -> Parser a -> Parser as
option a b = Parser $ \s -> case parse a s of
    []   -> parse b s
    res  -> res


