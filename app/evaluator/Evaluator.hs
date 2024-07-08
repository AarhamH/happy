 {-# LANGUAGE UnicodeSyntax #-}

module Evaluator where 
import Values
import Operators
import Control.Monad.Except

apply :: String -> [Values] -> ThrowsError Values
apply f a = maybe (throwError $ NotFunction "Unrecognized primitive function args" f) ($ a) (lookup f operators)

evaluateExpr :: Values -> ThrowsError Values
evaluateExpr val@(String _) = return val
evaluateExpr val@(Number _) = return val
evaluateExpr val@(Bool _) = return val
evaluateExpr (List [Atom "quote", val]) = return val
evaluateExpr (List [Atom "if", p, c, a]) = 
     do result <- evaluateExpr p
        case result of
             Bool False -> evaluateExpr a
             _  -> evaluateExpr c
evaluateExpr (List (Atom func : args)) = mapM evaluateExpr args >>= apply func 
evaluateExpr badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm