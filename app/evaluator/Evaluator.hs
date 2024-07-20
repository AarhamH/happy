module Evaluator where 
import Values
import Operators
import Control.Monad.Except
import Variables

apply :: String -> [Values] -> ThrowsError Values
apply f a = maybe (throwError $ NotFunction "Unrecognized primitive function args" f) ($ a) (lookup f operators)

evaluateExpr :: IOEnvironment -> Values -> IOThrowsError Values
evaluateExpr env val@(String _) = return val
evaluateExpr env val@(Number _) = return val
evaluateExpr env val@(Atom id) = getVar env id
evaluateExpr env val@(Bool _) = return val
evaluateExpr env (List [Atom "quote", val]) = return val
evaluateExpr env (List [Atom "if", p, c, a]) = 
     do result <- evaluateExpr env p
        case result of
             Bool False -> evaluateExpr env a
             _  -> evaluateExpr env c
evaluateExpr env (List [Atom "set!", Atom var, form]) = evaluateExpr env form >>= setVar env var
evaluateExpr env (List [Atom "define", Atom var, form]) = evaluateExpr env form >>= defineVar env var
evaluateExpr env (List (Atom func : args)) = mapM (evaluateExpr env) args >>= liftThrows . apply func
evaluateExpr env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm