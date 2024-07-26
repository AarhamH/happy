module Symbols where
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Maybe (fromMaybe)

operatorSymbols :: [(String, String)]
operatorSymbols = [
  ("true", "truthful"),
  ("false", "falseful"),
  ("add", "add_numbers"),
  ("subtract", "subtract_numbers"),
  ("multiply", "multiply_numbers"),
  ("divide", "divide_numbers"),
  ("modulo", "obtain_modulu"),
  ("remainder", "obtain_remainder"),
  ("strong_equal", "is_equal_with_strength"),
  ("weak_equal", "is_equal_with_weakness"),
  ("not_equal", "is_not_equal"),
  ("less_than_eq", "is_less_than_or_equal_to"),
  ("greater_than_eq", "is_greater_than_or_equal_to"),
  ("less_than", "is_less_than"),
  ("greater_than", "is_greater_than"),
  ("and", "and" ),
  ("or", "or"),
  ("strong_equal_str", "strings_is_equal_with_strength"),
  ("not_equal_str", "strings_is_not_equal_to"),
  ("less_than_eq_str", "strings_is_less_than_or_equal_to"),
  ("greater_than_eq_str", "strings_is_less_than_or_equal_to"),
  ("less_than_str", "strings_is_less_than"),
  ("greater_than_str", "strings_is_greater_than"),
  ("head", "obtain_head_of_list"),
  ("tail", "obtain_tail_of_list"),
  ("cons", "build_list_with")
  ]

symbol :: Parser Char
symbol = oneOf "[]!#$%&|*+-/:<=>?@^_~a"

lookupSymbol :: String -> String
lookupSymbol key = fromMaybe "" (lookup key operatorSymbols)
