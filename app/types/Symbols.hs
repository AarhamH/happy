module Symbols where
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Maybe (fromMaybe)

operatorSymbols :: [(String, String)]
operatorSymbols = [
  ("true", "👍"),
  ("false", "👎"),
  ("add", "➕"),
  ("subtract", "➖"),
  ("multiply", "✖️"),
  ("divide", "➗"),
  ("modulo", "🕒"),
  ("remainder", "🍕"),
  ("strong_equal", "🟰💪"),
  ("weak_equal", "🟰😩"),
  ("not_equal", "🚫"),
  ("less_than_eq", "🟰️⬇️"),
  ("greater_than_eq", "🟰️⬆️"),
  ("less_than", "⬇️"),
  ("greater_than", "⬆️"),
  ("and", "🌑" ),
  ("or", "🌕"),
  ("strong_equal_str", "🧵[🟰💪]"),
  ("not_equal_str", "🧵[🚫]"),
  ("less_than_eq_str", "🧵[🟰️⬇️]"),
  ("greater_than_eq_str", "🧵[🟰⬆️]"),
  ("less_than_str", "🧵[⬇️]"),
  ("greater_than_str", "🧵[⬆️]"),
  ("head", "🧠"),
  ("tail", "🍤"),
  ("cons", "🛠️")
  ]

symbol :: Parser Char
symbol = oneOf "[]!#$%&|*+-/:<=>?@^_~a🛠️🍤🧠👍👎➕➖✖️➗🕒🍕💪😩🚫⬇️⬆️🧵"

lookupSymbol :: String -> String
lookupSymbol key = fromMaybe "" (lookup key operatorSymbols)