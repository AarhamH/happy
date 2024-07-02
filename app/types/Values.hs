module Values where

import Control.Monad.Except
import Text.ParserCombinators.Parsec ( ParseError )

data Values = Atom String
            | List [Values]
            | ImproperList [Values] Values
            | Number Integer
            | String String
            | Bool Bool

data Errors = ArgumentNumber Integer [Values]
               | TypeMismatch String Values
               | Parser ParseError
               | BadSpecialForm String Values
               | NotFunction String String
               | UnboundVar String String
               | Default String

unwordsList :: [Values] -> String
unwordsList = unwords . map showValue 

showValue :: Values -> String 
showValue (Atom name) = name
showValue (String contents) = "\"" ++ contents ++ "\""
showValue (Number contents) = show contents
showValue (List contents) = "(" ++ unwordsList contents ++ ")"
showValue (ImproperList lhead ltail) = "(" ++ unwordsList lhead ++ " . " ++ showValue ltail ++ ")"
showValue (Bool True) = "#t"
showValue (Bool False) = "#f"

showError :: Errors -> String
showError (UnboundVar msg var) = msg ++ ": " ++ var
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form
showError (NotFunction msg f) = msg ++ ": " ++ show f
showError (ArgumentNumber expect found) = "Expected " ++ show expect ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expect found) = "Invalid type: expected " ++ expect ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Default err) = "Error: " ++ err

instance Show Values where show = showValue
instance Show Errors where show = showError

type ThrowsError = Either Errors

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left err) = error $ "Error: " ++ show err