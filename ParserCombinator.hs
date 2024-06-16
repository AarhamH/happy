
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