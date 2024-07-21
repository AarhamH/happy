module Values where

import Control.Monad.Except
import Text.ParserCombinators.Parsec ( ParseError )

import Data.IORef
import qualified Data.Functor
import qualified Data.Maybe
import GHC.IO.Handle
import Symbols

data Values = Atom String
            | List [Values]
            | ImproperList [Values] Values
            | Number Integer
            | String String
            | Bool Bool
            | PrimitiveFunc ([Values] -> ThrowsError Values)
            | Func { params :: [String], vararg :: Maybe String, body :: [Values], closure :: IOEnvironment }
            | IOFunc ([Values] -> IOThrowsError Values)
            | Port Handle

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
showValue (Bool True) = lookupSymbol "true"
showValue (Bool False) = lookupSymbol "false"
showValue (PrimitiveFunc _) = "<primitive>"
showValue (Func {params = args, vararg = varargs, body = _, closure = _}) =
   "(λ (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"
showValue (Port _) = "<IO Port>"
showValue (IOFunc _) = "<IO primitive>"

showError :: Errors -> String
showError (UnboundVar msg var) = msg ++ ": " ++ var
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form
showError (NotFunction msg f) = msg ++ ": " ++ show f
showError (ArgumentNumber expect found) = "Oh no, we want " ++ show expect ++ " args 🤭; found values " ++ unwordsList found
showError (TypeMismatch expect found) = "Oops! Wrong type 🤭: expected " ++ expect ++ ", found " ++ show found
showError (Parser parseErr) = "Sorry! Cannot parse 🤭" ++ show parseErr
showError (Default err) = "There is an error 🤭: " ++ err

instance Show Values where show = showValue
instance Show Errors where show = showError

type ThrowsError = Either Errors

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left err) = error $ "Error: " ++ show err

type IOEnvironment  = IORef [(String, IORef Values)]

nullEnv :: IO IOEnvironment
nullEnv = newIORef []

type IOThrowsError = ExceptT Errors IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) Data.Functor.<&> extractValue

isBound :: IOEnvironment -> String -> IO Bool
isBound envRef var = readIORef envRef Data.Functor.<&> (Data.Maybe.isJust . lookup var)